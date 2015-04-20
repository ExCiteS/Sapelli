/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.BelongsToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LinksToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * Abstract Controller class
 *
 * @param <CUI>
 * 
 * @author mstevens, Michalis Vitos, Julia
 */
public abstract class Controller<CUI extends CollectorUI<?, ?>> implements FieldVisitor
{
	
	// STATICS-------------------------------------------------------
	protected static final String LOG_PREFIX = "Collector_";
	public static final int VIBRATION_DURATION_MS = 600;
	
	/**
	 * The mode in which a {@link Form} is opened
	 */
	public static enum Mode
	{
		CREATE,
		EDIT,
		//SELECT
	}
	
	/**
	 * Determines what should happen when the current field is (attempted to be) left. 
	 */
	public static enum LeaveRule
	{
		/**
		 * Leaving the current field will only be allowed if validation (& value storage) is successful.
		 */
		CONDITIONAL,
		
		/**
		 * The current field must be unconditionally left but valid values may be stored.
		 */
		UNCONDITIONAL_WITH_STORAGE,
		
		/**
		 * The current field must be unconditionally left without any validation or value storage happening. 
		 */
		UNCONDITIONAL_NO_STORAGE
	}
	
	/**
	 * Checks whether the given field is to be shown in the current Mode.
	 * Note that disabled fields may still be shown (e.g. displayed grayed-out).
	 * 
	 * @param mode
	 * @param field
	 * @return
	 */
	static public boolean IsFieldToBeShown(Mode mode, Field field)
	{
		switch(mode)
		{
			case CREATE:
				return field.isShowOnCreate();
			case EDIT:
				return field.isShowOnEdit();
			default:
				throw new IllegalStateException("Unknown Mode: " + mode.name());
		}
	}
	
	/**
	 * Checks whether the given field is currently enabled.
	 * While disabled fields may still be shown, a field that is *not* allowed to be shown is always disabled.
	 * 
	 * @param currFormSession
	 * @param field
	 * @return
	 */
	static public boolean IsFieldEnabled(FormSession currFormSession, Field field)
	{
		return IsFieldEnabled(currFormSession.mode, currFormSession.getRuntimeEnabled(field), field);
	}
	
	/**
	 * Checks whether the given field is currently enabled.
	 * While disabled fields may still be shown, a field that is *not* allowed to be shown is always disabled.
	 * 
	 * @param mode
	 * @param field
	 * @return
	 */
	static public boolean IsFieldEnabled(Mode mode, Field field)
	{
		return IsFieldEnabled(mode, null, field);
	}
	
	/**
	 * Checks whether the given field is currently enabled.
	 * While disabled fields may still be shown, a field that is *not* allowed to be shown is always disabled.
	 * 
	 * @param mode
	 * @param runtimeEnabled
	 * @param field
	 * @return
	 */
	static public boolean IsFieldEnabled(Mode mode, Boolean runtimeEnabled, Field field)
	{
		return 	// a field that is *not* allowed to be shown is always disabled:
				IsFieldToBeShown(mode, field)
				// "runtime enabledness" (kept in FormSession, but rarely used) has preference over "static enabledness" (kept in the Field object itself, true by default):
				&& (runtimeEnabled != null ? runtimeEnabled : field.isEnabled())
				// when in EDIT mode the field must be editable to be enabled:
				&& (mode != Mode.EDIT || field.isEditable());
	}
	
	// DYNAMICS------------------------------------------------------
	protected final Project project;
	protected final CUI ui;
	protected final ProjectStore projectStore;
	protected final RecordStore recordStore;
	protected final FileStorageProvider fileStorageProvider;
	protected Logger logger;
	
	protected Stack<FormSession> formHistory;
	protected FormSession currFormSession;
	protected FormSession prevFormSession; 
	
	protected boolean handlingUserGoBackRequest = false;
	
	protected volatile boolean blockedUI = false;

	public Controller(Project project, CUI ui, ProjectStore projectStore, RecordStore recordStore, FileStorageProvider fileStorageProvider)
	{
		this.project = project;
		this.ui = ui;
		this.projectStore = projectStore;
		this.recordStore = recordStore;
		this.fileStorageProvider = fileStorageProvider;
		
		// Collections:
		formHistory = new Stack<FormSession>();
	}

	public void startProject()
	{
		if(project.isLogging())
		{
			try
			{
				logger = createLogger();

				// Log the DeviceID
				logger.addLine("DeviceID (CRC32)", String.valueOf(getDeviceID()));
				logger.addBlankLine();
	
				// Log the start of the project
				logger.addLine("PROJECT_START", project.toString());
				logger.addBlankLine();
			}
			catch(FileStorageException fse)
			{
				fse.printStackTrace(System.err);
			}
			catch(IOException ioe)
			{
				ioe.printStackTrace(System.err);
			}
		}
		
		// Clear/reset:
		prevFormSession = null;
		currFormSession = null;
		formHistory.clear();
		handlingUserGoBackRequest = false;
		
		// Open a Create-mode session for the startForm:
		openFormSession(FormSession.Create(project.getStartForm(), this));
	}

	protected void openFormSession(FormSession formSession)
	{
		openFormSession(formSession, false);
	}
	
	protected void openFormSession(FormSession formSession, boolean resumeForm)
	{
		// Deal with current form session:
		if(currFormSession != null)
		{
			disableTriggers(currFormSession.form.getTriggers()); // disable triggers
			prevFormSession = currFormSession; // remember previous formSession (always)
			if(!resumeForm &&								// If we are not "coming back",
			   currFormSession.form != formSession.form &&	// AND we are not looping within the same form,
			   !currFormSession.form.isSkipOnBack())		// AND the previous form does not have skipOnBack=true,
				formHistory.push(currFormSession);			// THEN: add previous formSession to history
		}
		currFormSession = formSession;
		currFormSession.setCurrentFieldDisplayed(false); //!!!
		
		// Log start form
		addLogLine("FORM_START", currFormSession.form.getName() + " (index: " + currFormSession.form.getPosition() + ")", "mode: " + currFormSession.mode.name());
		
		// Location...
		List<LocationField> lfStartWithForm = currFormSession.form.getLocationFields(true);
		if(!lfStartWithForm.isEmpty())
			startLocationListener(lfStartWithForm); // start listening for location updates
		else
			stopLocationListener(); // stop listening for location updates (if we were still listening for another form for example)
		
		// Setup the triggers
		setupTriggers(currFormSession.form.getTriggers());

		// Go to field...
		if(resumeForm && currFormSession.atField())
			goTo(currFormSession.getCurrent()); // continue where we left off
		else
			goTo(new FieldWithArguments(currFormSession.form.getStartField())); // begin filling out the form at the start field
	}
	
	public void cancelAndRestartForm()
	{	
		goTo(new FieldWithArguments(new EndField(currFormSession.form, false, Next.LOOPFORM)), LeaveRule.UNCONDITIONAL_NO_STORAGE); // loop without saving first (forced leaving of current field)
	}

	public void cancelAndStop()
	{
		goTo(new FieldWithArguments(new EndField(currFormSession.form, false, Next.EXITAPP)), LeaveRule.UNCONDITIONAL_NO_STORAGE); // exit without saving first (forced leaving of current field)
	}
	
	public boolean goToPreviousForm()
	{
		if(formHistory.empty())
			return false;
		//else:
		openFormSession(formHistory.pop(), true); // re-open previous form
		return true;
	}

	/**
	 * Go forward to next field (either the one below the current one or the one it jumps to)
	 * 
	 * @param requestedByUser
	 */
	public void goForward(boolean requestedByUser)
	{
		advance(requestedByUser, true); // jump is allowed
	}
	
	/**
	 * Advance to the field below the current field, or, if allowJump is true, the one the current field jumps to.
	 * 
	 * @param requestedByUser
	 * @param allowJump
	 */
	protected void advance(boolean requestedByUser, boolean allowJump)
	{
		if(handlingUserGoBackRequest && !requestedByUser)
		{	// We are currently handling a user *back* request and this is an automatic *forward* request, then we should be back instead of forward!
			goBack(false);
		}
		else
		{	// Normal going forward:
			if(currFormSession.atField())
				goTo(currFormSession.form.getNextFieldAndArguments(getCurrentField(), allowJump));
			else
				openFormSession(currFormSession); // this shouldn't happen really...
		}
	}

	/**
	 * Go back to previous field or form
	 * 
	 * @param requestedByUser
	 */
	public void goBack(boolean requestedByUser)
	{
		if(requestedByUser)
			// Remember we are handling a user initiated goBack request, this will turn subsequently triggered automatic goForward requests into goBack requests!
			handlingUserGoBackRequest = true; // Do *not* replace this by: handlingGoBackRequest = requestedByUser
		
		// Try to go to previous field...
		if(currFormSession.canGoBack())
			goTo(currFormSession.getPrevious(true), LeaveRule.UNCONDITIONAL_WITH_STORAGE); // force leaving but allow storage (if valid)
		else
			// Try to go to previous form...
			goToPreviousForm();
		
		// Reset user go back request flag:
		if(requestedByUser)
			handlingUserGoBackRequest = false;
	}
	
	/**
	 * Return whether the controller is moving backwards (or forwards), by user request.
	 * 
	 * @return true if going back and false otherwise
	 */
	public boolean isGoingBack()
	{
		return handlingUserGoBackRequest;
	}

	/**
	 * @param withinFormOnly
	 * @return whether of not we can go back to a previous field or (if withinFormOnly=false) form
	 */
	public boolean canGoBack(boolean withinFormOnly)
	{
		return (currFormSession != null && currFormSession.canGoBack()) || (!withinFormOnly && !formHistory.empty());
	}
	
	/**
	 * Re-enter current field
	 */
	public void goToCurrent(LeaveRule leaveRule)
	{
		goTo(currFormSession.getCurrent(), leaveRule);
	}
	
	public void goToCurrent(FieldParameters arguments, LeaveRule leaveRule)
	{
		goTo(new FieldWithArguments(currFormSession.getCurrentField(), arguments), leaveRule);
	}
	
	public void goTo(FieldWithArguments nextFieldAndArguments)
	{
		goTo(nextFieldAndArguments, LeaveRule.CONDITIONAL); // only leave upon successful validation (& value storage)
	}
	
	/**
	 * Go to the given field with arguments
	 * 
	 * @param nextField
	 * @param leaveRule determines what should happen when attempting to leave the current field 
	 */
	public synchronized void goTo(FieldWithArguments nextFieldAndArguments, LeaveRule leaveRule)
	{
		// Null check...
		if(nextFieldAndArguments == null || nextFieldAndArguments.field == null)
		{	
			addLogLine("NULL_FIELD");
			return;
		}
	
		// Try to leave the currently displayed field...
		if(currFormSession.atField() && currFormSession.isCurrentFieldDisplayed() && !ui.getCurrentFieldUI().leaveField(currFormSession.record, leaveRule))
		{
			addLogLine("STAY", "Not allowed to leave field " + getCurrentField().id);
			return; // not allowed to leave
		}
		
		// Next field becomes the (new) current field...
		currFormSession.setCurrent(nextFieldAndArguments); // deals with history as well
		
		// Temp variable for the new current field (avoids calling getters, and used to check whether another goForward/goTo call happens from the enter() method below):
		Field currField = currFormSession.getCurrentField();
		
		// Skip the new current field if it is not meant to be shown in the current form mode:
		if(!isFieldToBeShown(currField))
		{
			addLogLine("SKIPPING", currField.id, "Not shown on " + currFormSession.mode.name());
			advance(false, false); // no jump allowed
			return;
		}
		
		// Entering new current field...
		addLogLine("REACHED", currField.id);
		boolean needsUIUpdate = currField.enter(this, currFormSession.getCurrentFieldArguments(), false); // pass arguments to enter()
		
		// UI update, if (still) needed:
		if(currFormSession.getCurrentField() == currField)
		{	// If the current field hasn't changed as a result of the enter() call...
			if(needsUIUpdate)
				setFieldInUI(currField); // update UI if needed
			currFormSession.setCurrentFieldDisplayed(needsUIUpdate); // remember whether current field is displayed
		}
		//else: when the current field *has* changed as part of the entering then we are done here
	}
	
	/**
	 * Can be overridden, e.g. to ensure the right (main/UI) thread is used
	 * 
	 * @param newCurrentField
	 */
	protected void setFieldInUI(Field newCurrentField)
	{
		ui.setField(newCurrentField);
	}
	
	/**
	 * Checks whether the given field is currently enabled.
	 * While disabled fields may still be shown, a field that is *not* allowed to be shown is always disabled.
	 * 
	 * @param field
	 * @return
	 */
	public boolean isFieldEnabled(Field field)
	{
		return IsFieldEnabled(currFormSession, field);
	}
	
	/**
	 * Checks whether the given field is to be shown in the current Mode.
	 * Note that disabled fields may still be shown (e.g. displayed grayed-out).
	 * 
	 * @param field
	 * @return
	 */
	public boolean isFieldToBeShown(Field field)
	{
		return IsFieldToBeShown(currFormSession.mode, field);
	}
	
	protected void saveRecordAndAttachments()
	{
		if(!currFormSession.form.isProducesRecords()) //!!!
			return;
				
		// Delete any files that were "queued" for deletion but not actually deleted yet:
		currFormSession.deleteDiscardedAttachments();
		
		// NOTE: no need to touch the added files since they were added on creation
		
		// Clear the list of added files so they cannot be deleted accidentally:
		currFormSession.clearAddedAttachments();
				
		// Finalise the currentRecord:
		currFormSession.form.finish(currFormSession.record); // (re)sets the end-time if necessary
	
		// Log record:
		addLogLine("RECORD", currFormSession.record.toString());
		
		// Store currentRecord:
		try
		{
			recordStore.store(currFormSession.record);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			addLogLine("ERROR", "Upon saving record", ExceptionHelpers.getMessageAndCause(e));
			return;
		}
		
		// Signal the successful storage of the currentRecord
		// Vibration
		if(currFormSession.form.isVibrateOnSave())
			vibrate(VIBRATION_DURATION_MS);
		// Play sound
		File endSoundFile = fileStorageProvider.getProjectSoundFile(project, currFormSession.form.getSaveSoundRelativePath());
		if(FileHelpers.isReadableFile(endSoundFile))
			playSound(endSoundFile);
	}
	
	/**
	 * Makes the record null & deletes any media attachments.
	 * 
	 * Note:
	 * 	Making the record null is necessary to avoid that unsaved foreign records are used
	 * 	(i.e. referred to with a foreign key value) when returning to a BelongsTo field in
	 * 	a previous form (see {@link #enterBelongsTo(BelongsToField, FieldParameters)}).
	 *  Doing so is risky however because an NPE will be thrown (likely crashing the app)
	 *  when some FieldUI or controller method attempts to (illegally!) use the record
	 *  after this discard operation. Obviously that shouldn't happen but we've had several
	 *  cases in which it did. However, all (known) cases have been resolved and any new
	 *  similar cases would be revealed soon by an NPE and/or crash. 
	 * 
	 */
	protected void discardRecordAndAttachments()
	{
		// delete any files that were added but have now been discarded:
		currFormSession.deleteAddedAttachments();
		
		// NOTE: no need to touch the deleted files since their deletion has been aborted
		
		// Clear the list of deleted files:
		currFormSession.clearDiscardedAttachments();
		
		// Discard record itself:
		currFormSession.record = null; // !!!
	}
	
	@Override
	public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments, boolean withPage)
	{
		if(withPage)
			return true;
		// else (not with page):
		// 	Deal with leaves:
		if(cf.isLeaf())
			return false; // this should never happen
		// Add the choice options to the log files
		addLogLine("CHOICE_OPTIONS", cf.getChildren().toString());
		// 	The UI needs to be updated to show this ChoiceField, but only is there is at least one enable (i.e. selectable) child:
		for(ChoiceField child : cf.getChildren())
			if(IsFieldEnabled(currFormSession, child))
				return true;
		// 	This ChoiceField currently has no enabled children, so we should skip it:
		goForward(false);
		return false;
	}
	
	@Override
	public boolean enterMediaField(MediaField mf, FieldParameters arguments, boolean withPage)
	{
		// we no longer skip media fields when max reached, so always return true
		return true;
	}
	
	/**
	 * @param lf  the LocationField
	 * @param whether or not the location field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	@Override
	public boolean enterLocationField(LocationField lf, FieldParameters arguments, boolean withPage)
	{
		if(withPage && !(lf.getStartWith() == LocationField.StartWith.PAGE))
			return false;
		
		if(lf.isWaitAtField() || /*try to use currentBestLocation:*/ !lf.storeLocation(currFormSession.record, getCurrentBestLocation()))
		{
			startLocationListener(lf); // start listening for a location
			return true;
		}
		else
		{	// we already have a (good enough) location
			if(!withPage)
				goForward(false); // skip the wait screen
			return false;
		}
	}
	
	@Override
	public boolean enterOrientationField(OrientationField of, FieldParameters arguments, boolean withPage)
	{
		if(!withPage)
			startOrientationListener();
		return true;
		// update UI (even though the orientation values are typically received instantaneously and the UI might never actually be seen by the user)
	}
	
	@Override
	public boolean enterPage(Page page, FieldParameters arguments)
	{
		// Enter child fields (but signal that they are entered as part of entering the page):
		for(Field f : page.getFields())
		{	
			if(!isFieldToBeShown(f))
				addLogLine("SKIPPING", f.id, "not shown on " + currFormSession.mode.name());
			else
				f.enter(this, FieldParameters.EMPTY, true); // enter with page (but don't pass on the arguments)
		}
		
		// Setup the triggers
		setupTriggers(page.getTriggers());

		return true;
	}
	
	@Override
	public boolean enterLinksTo(LinksToField linksTo, FieldParameters arguments)
	{
		//TODO enterLinksTo
		
//		Record foreignRecord = getHeldRecord(rel);
//		if(foreignRecord != null)
//			openFormSession(FormSession.Edit(rel.getRelatedForm(), foreignRecord)); // Edit the "held" record
//		else
//			openFormSession(FormSession.Create(rel.getRelatedForm(), deviceIDHash)); ; // Open related from to create a new record
		return false;
	}
	
	@Override
	public boolean enterBelongsTo(BelongsToField belongsTo, FieldParameters arguments)
	{
		ForeignKeyColumn column = belongsTo.getColumn();
		Constraint constraints = belongsTo.getConstraints();
		RecordReference foreignKey = column.retrieveValue(currFormSession.record); // foreignKey may be null
		
		if(!arguments.getBoolean(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, false))
		{	// We were *not* waiting for a return from the relatedForm
			// Check is we already have a value...
			if(foreignKey != null)
			{	// We already have a foreign key value
				if(arguments.getBoolean(BelongsToField.PARAMETER_EDIT, false))
				{	// We are in edit mode (the edit argument was true):
					arguments.put(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, Boolean.TRUE.toString()); // remember we are waiting for relatedForm
					openFormSession(FormSession.Edit(belongsTo.getRelatedForm(), recordStore.retrieveRecord(foreignKey.getRecordQuery()), this)); // open relatedForm to edit foreign record
				}
				else
					// We are not in edit mode (the edit argument was false, or more likely, missing)
					goForward(false); // continue to next field
			}
			else
			{	// We don't have a foreign key value yet
				//	Note: we ignore the edit argument here because we only allow editing if a value is already set
				Record foreignRecord = null;
				// Check is we are allowed to hold on to foreign records:
				if(belongsTo.isHoldForeignRecord())
				{	// The Relationship is allowed to hold on to foreign records 
					RecordReference heldForeignKey = projectStore.retrieveHeldForeignKey(belongsTo);
					foreignRecord = heldForeignKey != null ? recordStore.retrieveRecord(heldForeignKey.getRecordQuery()) : null;
					if(constraints.isValid(foreignRecord)) // passing null will return false
					{	// We have a "held" foreign key, the corresponding foreign record was found and meets the constraints
						column.storeValue(currFormSession.record, heldForeignKey); // Store foreign key
						goForward(false); // continue to next field
					}
					else
					{	// Either we didn't have a "held" foreign key, OR no corresponding record was found, OR the record didn't meet the constraints
						projectStore.deleteHeldForeignKey(belongsTo); // clear held foreign key (if there was none nothing will happen)
						foreignRecord = null; // relatedForm will be opened for creation below 
					}
				}
				if(foreignRecord == null)
				{	// We didn't find a valid held foreign record or the relationship is simply *not* allowed to hold on to foreign records
					arguments.put(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, Boolean.TRUE.toString()); // remember we are waiting for relatedForm
					openFormSession(FormSession.Create(belongsTo.getRelatedForm(), this)); // open relatedForm to create new record
				}
			}
		}
		else
		{	// We were waiting to return from the relatedForm...
			// Clear waitingForRelatedForm parameter:
			arguments.clear(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM);
			// Check if we really came back from the relatedForm:
			if(prevFormSession != null && prevFormSession.form == belongsTo.getRelatedForm())
			{	// ... yes we did.
				Record foreignRecord = prevFormSession.record;
				if(constraints.isValid(foreignRecord)) // passing null will return false
				{	// The relatedForm produced/edited a non-null record which meets the constraints
					foreignKey = new RecordReference(foreignRecord);
					column.storeValue(currFormSession.record, foreignKey); // Store/update foreign key
					if(belongsTo.isHoldForeignRecord())
						projectStore.storeHeldForeignKey(belongsTo, foreignKey); // Store/update "held" foreign key if allowed
					goForward(true); // continue to next field
				}
				else
				{	// Either the relatedForm did not save its record (i.e. it is now null), OR it doesn't meet the constraints
					if(foreignKey != null || belongsTo.isOptional())
						// Either we already have a (previously set) foreign key value, OR we don't need one because the field is optional
						goForward(true); // continue to next field (keeping the currently stored foreign key if there is one, or keeping it blank if there is none)
					
					else
						// We do not already have a foreign key value & the field is not optional
						openFormSession(FormSession.Create(belongsTo.getRelatedForm(), this)); // re-open relatedForm to create new record
				}
			}
			else
			{	// we were waiting to return from relatedForm but the previous form is another one: this should never happen(?)
				// TODO show error & restartForm?
			}
		}
		
		// TODO "reset starttime upon leave"? (would need to happen at every goForward() call)
		
		return false;
	}
	
	@Override
	public boolean enterTextBoxField(TextBoxField tbf, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	@Override
	public boolean enterLabelField(LabelField lblf, FieldParameters arguments, boolean withPage)
	{
		return true;
	}
	
	@Override
	public boolean enterCheckboxField(CheckBoxField cbf, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	@Override
	public boolean enterButtonField(ButtonField buttonField, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	@Override
	public boolean enterMultiListField(MultiListField mlf, FieldParameters arguments, boolean withPage)
	{
		return true;
	}
	
	@Override
	public boolean enterEndField(EndField ef, FieldParameters arguments)
	{
		// Logging:
		addLogLine("FORM_END", ef.id, currFormSession.form.getName(), Long.toString((getElapsedMillis() - currFormSession.startTime) / 1000) + " seconds");
		
		// Save or discard:
		if(ef.isSave() && currFormSession.form.isProducesRecords())
			saveRecordAndAttachments();
		else
			discardRecordAndAttachments();
		
		// Insert blank line in log:
		addBlankLogLine();
		
		// Go to "next":
		switch(ef.getNext())
		{
			case LOOPFORM:
				openFormSession(FormSession.Create(currFormSession.form, this));
				break;
			case LOOPPROJ:
				startProject(); // formHistory & currFormSession will be cleared
				break;
			case PREVFORM:
				if(!goToPreviousForm()) // try to re-open previous form
				{	// there is no previous form (this shouldn't really happen...):
					showError("Invalid state: no previous form to return to!", false); //TODO multilang
					startProject(); // restart project instead
				}
				break;
			case NEXTFORM:
				Form nextForm = project.getNextForm(currFormSession.form);
				if(nextForm != null)
					openFormSession(FormSession.Create(nextForm, this));
				else
				{	// there is no next form:
					showError("Invalid state: there is no next form to go to from here!", false); //TODO multilang
					startProject(); // restart project instead
				}
				break;
			case EXITAPP:
				exit(true); // exit controller & application
				break;
		}
		// No UI update needed:
		return false;
	}

	/**
	 * Set-up the given triggers.
	 * 
	 * @param triggers
	 */
	private void setupTriggers(List<Trigger> triggers)
	{
		for(Trigger trigger : triggers)
			setupTrigger(trigger);
	}
	
	/**
	 * Disable the given triggers.
	 * 
	 * @param triggers
	 */
	public void disableTriggers(List<Trigger> triggers)
	{
		for(Trigger trigger : triggers)
			disableTrigger(trigger);
	}
	
	/**
	 * Set-up the given trigger
	 * 
	 * @param trigger
	 */
	protected void setupTrigger(Trigger trigger)
	{
		// Key press trigger:
		if(!trigger.getKeys().isEmpty())
		{
			setupKeyPressTrigger(trigger);
			addLogLine("TRIGGER", "Set-up key press trigger, firing on pressing of " + CollectionUtils.allToString(trigger.getKeys(), false));
		}		
		// Fixed timer trigger:
		if(trigger.getFixedTimer() != Trigger.NO_TIMEOUT)
		{
			if(logger != null)
				logger.addLine("TRIGGER", "Set-up fixed timer trigger, firing in " + trigger.getFixedTimer() + " seconds");			
			setupTimerTrigger(trigger);
		}
	}
	
	protected abstract void setupKeyPressTrigger(Trigger trigger);
	
	protected abstract void setupTimerTrigger(Trigger trigger);
	
	/**
	 * Disable the given trigger
	 * 
	 * @param trigger
	 */
	protected void disableTrigger(Trigger trigger)
	{
		// Key press trigger:
		if(!trigger.getKeys().isEmpty())
		{
			disableKeyPressTrigger(trigger);
			addLogLine("TRIGGER", "Disabled key press trigger, firing on pressing of " + CollectionUtils.allToString(trigger.getKeys(), false));
		}
		// Fixed timer trigger:
		if(trigger.getFixedTimer() != Trigger.NO_TIMEOUT)
		{
			if(logger != null)
				logger.addLine("TRIGGER", "Disabled fixed timer trigger");
			disableTimerTrigger(trigger);
		}	
	}
	
	protected abstract void disableKeyPressTrigger(Trigger trigger);
	
	protected abstract void disableTimerTrigger(Trigger trigger);
	
	/**
	 * Execute trigger
	 * 
	 * @param trigger
	 */
	public void fireTrigger(Trigger trigger)
	{
		if(trigger.getJump() == null)
			return;
		addLogLine("TRIGGER", "Fired, jumping to: " + trigger.getJump().id);
		goTo(new FieldWithArguments(trigger.getJump(), trigger.getNextFieldArguments()));
	}
	
	/**
	 * Stops all use/activities of the controller but does not exit the containing application.
	 * I.e. the controller can still be restarted!
	 */
	public void discard()
	{
		// Logging:
		addLogLine("FORM_END", "CONTROLLER_DISCARD", currFormSession.form.getName(), Long.toString((getElapsedMillis() - currFormSession.startTime) / 1000) + " seconds");
		
		// Save nothing:
		discardRecordAndAttachments();
		
		// Exit controller (but not the application):
		exit(false);
	}
	
	/**
	 * Exit controller & optionally the containing application
	 * 
	 * @param exitApp
	 */
	protected void exit(boolean exitApp)
	{
		// Cancel (timer) triggers:
		if(currFormSession.form != null)
			disableTriggers(currFormSession.form.getTriggers());
		
		// Stop sensors:
		stopLocationListener();
		stopOrientationListener();
		
		// Close log file:
		if(logger != null)
		{
			logger.addFinalLine("EXIT_COLLECTOR", project.getName(), currFormSession.form.id); // closes the logger & underlying file(writer)
			logger = null;
		}

		if(exitApp)
			exitApp();
	}

	/**
	 * @return the project
	 */
	public Project getProject()
	{
		return project;
	}

	/**
	 * @return the currentRecord
	 */
	public Record getCurrentRecord()
	{
		return currFormSession.record;
	}
	
	/**
	 * @return the current Form
	 */
	public Form getCurrentForm()
	{
		return currFormSession.form;
	}
	
	/**
	 * @return the mode of the currently open form
	 */
	public Mode getCurrentMode()
	{
		return currFormSession.mode;
	}

	/**
	 * @return the current Field
	 */
	public Field getCurrentField()
	{
		return currFormSession.getCurrentField();
	}
	
	/**
	 * @return the current FieldArguments
	 */
	public FieldParameters getCurrentFieldArguments()
	{
		return currFormSession.getCurrentFieldArguments();
	}
	
	public void addLogLine(String... fields)
	{
		if(logger != null)
			logger.addLine(fields);
	}
	
	public void addBlankLogLine()
	{
		if(logger != null)
			logger.addBlankLine();
	}
	
	public FileStorageProvider getFileStorageProvider()
	{
		return fileStorageProvider;
	}
	
	public void addAttachment(File file) {
		currFormSession.addAttachment(file);
	}
	
	public void discardAttachment(File file) {
		currFormSession.discardAttachment(file);
	}

	protected void startLocationListener(LocationField locField)
	{
		startLocationListener(Arrays.asList(locField));
	}

	protected abstract void startOrientationListener();
	
	protected abstract void stopOrientationListener();
	
	protected abstract void startLocationListener(List<LocationField> locFields);

	protected abstract void stopLocationListener();
	
	public abstract Location getCurrentBestLocation();
	
	protected abstract void vibrate(int durationMS);
	
	protected abstract void playSound(File soundFile);
	
	protected abstract void showError(String errorMsg, boolean exit);
	
	protected abstract void exitApp();
	
	protected abstract long getDeviceID();
	
	/**
	 * Returns the number of milliseconds since system boot.<br/>
	 * Should use a realtime monotonic clock, i.e. independent of time(zone) changes, deep sleep, CPU power saving, etc.
	 * 
	 * @return number of milliseconds since system boot
	 */
	protected abstract long getElapsedMillis();
	
	/**
	 * @return the blockedUI
	 */
	public synchronized boolean isUIBlocked()
	{
		return blockedUI;
	}

	/**
	 * Block UI
	 */
	public synchronized void blockUI()
	{
		this.blockedUI = true;
	}

	/**
	 * Unblock UI
	 */
	public synchronized void unblockUI()
	{
		this.blockedUI = false;
	}
	
	protected Logger createLogger() throws FileStorageException, IOException
	{
		return new Logger(fileStorageProvider.getProjectLogsFolder(project, true).getAbsolutePath(), LOG_PREFIX, true);
	}
	
}
