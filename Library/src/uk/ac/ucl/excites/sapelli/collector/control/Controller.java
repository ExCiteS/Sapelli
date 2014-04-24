package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormSession.Mode;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.BelongsToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LinksToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * Abstract Controller class
 * 
 * @author mstevens, Michalis Vitos, Julia
 */
public abstract class Controller
{
	
	// STATICS-------------------------------------------------------
	private static final String LOG_PREFIX = "Collector_";
	public static final int VIBRATION_DURATION_MS = 600;
	
	// DYNAMICS------------------------------------------------------
	protected Project project;
	protected CollectorUI<?, ?> ui;
	protected ProjectStore projectStore;
	protected RecordStore recordStore;
	protected Logger logger;
	
	protected long deviceIDHash; //to be initialised by subclasses
	
	protected Stack<FormSession> formHistory;
	protected FormSession currFormSession;
	protected FormSession prevFormSession; 
	
	protected boolean handlingUserGoBackRequest = false;
	
	public Controller(Project project, CollectorUI<?, ?> ui, ProjectStore projectStore, RecordStore recordStore)
	{
		this.project = project;
		this.ui = ui;
		this.projectStore = projectStore;
		this.recordStore = recordStore;
		
		// Collections:
		formHistory = new Stack<FormSession>();
	}

	public void startProject()
	{
		if(project.isLogging())
		{
			try
			{
				logger = new Logger(project.getLogFolder().getAbsolutePath(), LOG_PREFIX);
	
				// Log the start of the project
				logger.addLine("PROJECT_START", project.toString());
				logger.addBlankLine();
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
		
		// Open a Create-mode session for the startForm:
		openFormSession(FormSession.Create(project.getStartForm(), deviceIDHash));
	}

	public void openFormSession(FormSession formSession)
	{
		openFormSession(formSession, false);
	}
	
	public void openFormSession(FormSession formSession, boolean resumeForm)
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
		
		// Log start form
		addLogLine("FORM_START", currFormSession.form.getName() + " (index: " + currFormSession.form.getPosition() + ")");
		
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
			goTo(currFormSession.currFieldAndArguments); // continue where we left off
		else
			goTo(new FieldWithArguments(currFormSession.form.getStartField())); // begin filling out the form at the start field
	}
	
	public void cancelAndRestartForm()
	{	
		goTo(new FieldWithArguments(new EndField(currFormSession.form, false, Next.LOOPFORM)), true); // loop without saving first (forced leaving of current field)
	}

	public void cancelAndStop()
	{
		goTo(new FieldWithArguments(new EndField(currFormSession.form, false, Next.EXITAPP)), true); // exit without saving first (forced leaving of current field)
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
	 * Go forward to next field
	 * 
	 * @param requestedByUser
	 */
	public void goForward(boolean requestedByUser)
	{
		if(handlingUserGoBackRequest && !requestedByUser)
		{
			goBack(false); // if we are currently handling a user *back* request and this is an automatic *forward* request, then we should be back instead of forward!
			return;
		}
		
		if(currFormSession.atField())
			goTo(currFormSession.form.getNextFieldAndArguments(getCurrentField()));
		else
			openFormSession(currFormSession); // this shouldn't happen really...
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
		if(!currFormSession.fieldAndArgumentHistory.isEmpty())
		{
			currFormSession.currFieldAndArguments = null; // !!! otherwise we create loops
			currFormSession.currFieldDisplayed = false;
			goTo(currFormSession.fieldAndArgumentHistory.pop());
		}
		else
			// Try to go to previous form...
			goToPreviousForm();
		
		// Reset user go back request flag:
		if(requestedByUser)
			handlingUserGoBackRequest = false;
	}
	
	public void goTo(FieldWithArguments nextFieldAndArguments)
	{
		goTo(nextFieldAndArguments, false);
	}
	
	/**
	 * @param nextField
	 * @param forceLeave when true the previous field is left without any validation nor storage(!), under the assumption the user will come back there. It is *not* the same as the noValidation argument on leave(Record,boolean).
	 */
	public synchronized void goTo(FieldWithArguments nextFieldAndArguments, boolean forceLeave)
	{
		// Null check...
		if(nextFieldAndArguments == null || nextFieldAndArguments.field == null)
		{	
			addLogLine("NULL_FIELD");
			return;
		}
	
		// Deal with current field...
		if(currFormSession.atField())
		{
			// Check if we are allowed to leave the current field...	
			if(!forceLeave && currFormSession.currFieldDisplayed && !ui.getCurrentFieldUI().leave(currFormSession.record)) // check if we are allowed to leave the currently displayed field (unless the leaving is forced)
			{
				addLogLine("STAY", "Not allowed to leave field " + getCurrentField().getID());
				return; // not allowed to leave
			}
			
			// Add current field to history if it is not the same as the next field & it is not to be skipped upon back...
			if(nextFieldAndArguments.field != getCurrentField() && !getCurrentField().isSkipOnBack())
				currFormSession.fieldAndArgumentHistory.push(currFormSession.currFieldAndArguments);
		}
		
		// 	Next field becomes the (new) current field...
		currFormSession.currFieldAndArguments = nextFieldAndArguments;
		
		// Skip the new current field if it is not meant to be shown on create/edit...
		if(	(currFormSession.mode == Mode.CREATE && !getCurrentField().isShowOnCreate()) ||
			(currFormSession.mode == Mode.EDIT && !getCurrentField().isShowOnEdit()))
		{
			addLogLine("SKIPPING", getCurrentField().getID(), "Not shown on " + currFormSession.mode.toString());
			goForward(false);
			return;
		}
		// Skip uneditable fields when in edit mode...
		if(currFormSession.mode == Mode.EDIT && !getCurrentField().isEditable())
		{
			addLogLine("SKIPPING", getCurrentField().getID(), "Not editable");
			goForward(false);
			return;
		}
		
		// Entering new current field...
		addLogLine("REACHED", getCurrentField().getID());
		Field holdCurrentField = getCurrentField(); // remember the "current" current field, to deal with case in which another goForward/goTo call happens from the enter() method (e.g. when entering a leaf choice)
		boolean needsUIUpdate = currFormSession.currFieldAndArguments.field.enter(this, currFormSession.currFieldAndArguments.arguments, false); // pass arguments to enter()
		// If the current field has changed as part of the entering we are done here: 
		if(getCurrentField() != holdCurrentField)
			return;
		//else: update UI if needed and remember whether current field is displayed
		if(needsUIUpdate)
			ui.setField(getCurrentField());
		currFormSession.currFieldDisplayed = needsUIUpdate;
	}
	
	/**
	 * Re-enter current field
	 */
	public void goToCurrent()
	{
		goTo(currFormSession.currFieldAndArguments, true); // force leaving
	}
	
	protected void saveRecordAndAttachments()
	{
		if(!currFormSession.form.isProducesRecords()) //!!!
			return;
		
		// Finalise the currentRecord:
		currFormSession.form.finish(currFormSession.record); // (re)sets the end-time if necessary
	
		// Store currentRecord
		recordStore.store(currFormSession.record);
	
		// Log record:
		addLogLine("RECORD", currFormSession.record.toString());
	
		// Move attachments from temp to data folder:
		try
		{
			File dataFolder = project.getDataFolder();
			for(File attachment : currFormSession.mediaAttachments)
				attachment.renameTo(new File(dataFolder.getAbsolutePath() + File.separator + attachment.getName()));
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace(System.err);
		}
	
		// Signal the successful storage of the currentRecord
		// Vibration
		if(currFormSession.form.isVibrateOnSave())
			vibrate(VIBRATION_DURATION_MS);
		// Play sound
		File endSoundFile = project.getSoundFile(currFormSession.form.getSaveSoundRelativePath());
		if(FileHelpers.isReadableFile(endSoundFile))
			playSound(endSoundFile);
	}
	
	protected void discardRecordAndAttachments()
	{
		// Discard record:
		currFormSession.record = null; //!!!
		
		// Delete any attachments:
		for(File attachment : currFormSession.mediaAttachments)
			if(attachment.exists())
				attachment.delete();
		currFormSession.mediaAttachments.clear();
	}
	
	/**
	 * @param cf  the ChoiceField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments)
	{
		// Deal with leaves:
		if(cf.isLeaf())
		{
			goForward(true); // go to next field (leaf will not be remembered on the fieldHistory stack because it has skipOnBack=true)
			return false;
		}
		// The UI needs to be updated to show this ChoiceField, but only is there is at least one enable (i.e. selectable) child:
		for(ChoiceField child : cf.getChildren())
			if(isFieldEndabled(child))
				return true;
		// This ChoiceField currently has no enabled children, so we should skip it:
		goForward(false);
		return false;
	}
	
	/**
	 * @param af  the MediaField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMediaField(MediaField mf, FieldParameters arguments)
	{
		if(mf.isMaxReached(currFormSession.record))
		{ // Maximum number of attachments for this field is reached:
			goForward(false); // skip field //TODO this needs to change if we allow to delete previously generated media
			return false;
		}
		return true;
	}
	
	/**
	 * @param lf  the LocationField
	 * @param whether or not the location field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterLocationField(LocationField lf, FieldParameters arguments, boolean withPage)
	{
		if(withPage && !lf.isStartWithPage())
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
	
	/**
	 * @param of  the OrientationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterOrientationField(OrientationField of, FieldParameters arguments);
	
	/**
	 * @param page	the Page
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterPage(Page page, FieldParameters arguments)
	{
		// Deal with returning from field that is (part of) the page itself:
		Field prevField = currFormSession.getPreviousField();
		while(prevField != null && (prevField == page || page.getFields().contains(prevField.getRoot())))
		{	// pop on-page field(s) & page itself off the history:
			currFormSession.fieldAndArgumentHistory.pop();
			prevField = currFormSession.getPreviousField();
		}
		
		// Enter child fields (but signal that they are entered as part of entering the page):
		for(Field f : page.getFields())
		{	
			if(	(currFormSession.mode == Mode.CREATE && !f.isShowOnCreate()) ||
				(currFormSession.mode == Mode.EDIT && !f.isShowOnEdit()))
				addLogLine("SKIPPING", getCurrentField().getID());
			else
				f.enter(this, FieldParameters.EMPTY, true); // enter with page (but don't pass on the arguments)
		}
		
		// Setup the triggers
		setupTriggers(page.getTriggers());

		return true;
	}
	
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
	
	public boolean enterBelongsTo(BelongsToField belongsTo, FieldParameters arguments)
	{
		ForeignKeyColumn column = belongsTo.getColumn();
		Constraint constraints = belongsTo.getConstraints();
		ForeignKey foreignKey = column.retrieveValue(currFormSession.record); // may be null
		
		if(!arguments.getBoolean(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, false))
		{	// We were *not* waiting for a return from the relatedForm
			// Check is we already have a value...
			if(foreignKey != null)
			{	System.out.println("we alreadt have a value");// We already have a foreign key value
				if(arguments.getBoolean(BelongsToField.PARAMETER_EDIT, false))
				{	System.out.println("edit mode");// in edit mode (the edit argument was true)
					//	Edit foreign record:
					arguments.put(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, Boolean.TRUE.toString()); // remember we are waiting for relatedForm
					openFormSession(FormSession.Edit(belongsTo.getRelatedForm(), recordStore.retrieveRecord(foreignKey.getForeignRecordQuery()))); // open relatedForm to edit foreign record
				}
				else
				{	System.out.println("not edit mode");// not in edit mode (the edit argument was false, or more likely, missing)
					goForward(false); // continue to next field
				}
			}
			else
			{	System.out.println("we don't have a value yet");// We don't have a foreign key value yet
				// Note: we ignore the edit argument here because we only allow editing if a value is already set
				Record foreignRecord = null;
				// Check is we are allowed to hold on to foreign records:
				if(belongsTo.isHoldForeignRecord())
				{	System.out.println("isHeldFR=true");// the Relationship is allowed to hold on to foreign records 
					ForeignKey heldForeignKey = projectStore.retrieveHeldForeignKey(belongsTo);
					foreignRecord = heldForeignKey != null ? recordStore.retrieveRecord(heldForeignKey.getForeignRecordQuery()) : null;
					if(constraints.isValid(foreignRecord)) // passing null will return false
					{	System.out.println("we have a valid held rec"); // we have a "held" foreign key, the corresponding foreign record was found and meets the constraints
						column.storeValue(currFormSession.record, heldForeignKey); // Store foreign key
						goForward(false); // continue to next field
					}
					else
					{	System.out.println("no valid held rec, held key: " + heldForeignKey + "; heldrec: " + foreignRecord);// Either we didn't have a "held" foreign key, OR no corresponding record was found, OR the record didn't meet the constraints
						projectStore.deleteHeldForeignKey(belongsTo); // clear held foreign key (if there was none nothing will happen)
						foreignRecord = null; // relatedForm will be opened for creation below 
					}
				}
				if(foreignRecord == null)
				{	System.out.println("no valid held record or not allowed to hold");// we didn't find a valid held foreign record or the relationship is simply *not* allowed to hold on to foreign records
					arguments.put(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM, Boolean.TRUE.toString()); // remember we are waiting for relatedForm
					openFormSession(FormSession.Create(belongsTo.getRelatedForm(), deviceIDHash)); // open relatedForm to create new record
				}
			}
		}
		else
		{	// We were waiting to return from the relatedForm...
			// Clear waitingForRelatedForm parameter:
			arguments.clear(BelongsToField.PARAMETER_WAITING_FOR_RELATED_FORM);
			// Check if we really came back from the relatedForm:
			if(prevFormSession != null && prevFormSession.form == belongsTo.getRelatedForm())
			{	// ... and we did indeed return from it
				Record foreignRecord = prevFormSession.record;
				
				if(constraints.isValid(foreignRecord)) // passing null will return false
				{	System.out.println("related form savid valid foreign rec");// the relatedForm produced/edited a non-null record which meets the constraints
					foreignKey = new ForeignKey(foreignRecord);
					column.storeValue(currFormSession.record, foreignKey); // Store/update foreign key
					if(belongsTo.isHoldForeignRecord())
						projectStore.storeHeldForeignKey(belongsTo, foreignKey); // Store/update "held" foreign key if allowed
					goForward(true); // continue to next field
				}
				else
				{	System.out.println("no valid foreign rec, fRec: " + foreignRecord);// either the relatedForm did not save its record (i.e. it is now null), OR it doesn't meet the constraints
					if(foreignKey != null || belongsTo.getOptional() == Optionalness.ALWAYS)
					{	// Either we already have a (previously set) foreign key value, OR we don't need one because the field is optional
						goForward(true); // continue to next field (keeping the currently stored foreign key if there is one, or keeping it blank if there is none)
					}
					else
					{	// We do not already have a foreign key value & the field is not optional
						openFormSession(FormSession.Create(belongsTo.getRelatedForm(), deviceIDHash)); // re-open relatedForm to create new record
					}
				}
			}
			else
			{	// we were waiting to return from related for but the previous form is another one: this should never happen(?)
				// TODO show error & restartForm?
			}
		}
		
		// TODO "reset starttime upon leave"? (would need to happen at every goForward() call)
		
		return false;
	}
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef, FieldParameters arguments)
	{
		// Logging:
		addLogLine("FORM_END", ef.getID(), currFormSession.form.getName(), Long.toString((System.currentTimeMillis() - currFormSession.startTime) / 1000) + " seconds");
		
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
				openFormSession(FormSession.Create(currFormSession.form, deviceIDHash));
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
					openFormSession(FormSession.Create(nextForm, deviceIDHash));
				else
				{	// there is no next form:
					showError("Invalid state: there is no next form to go to from here!", false); //TODO multilang
					startProject(); // restart project instead
				}
				break;
			case EXITAPP:
				exit();
				break;
		}
		// No UI update needed:
		return false;
	}

	private void setupTriggers(List<Trigger> triggers)
	{
		for(Trigger trigger : triggers)
			setupTrigger(trigger);
	}
	
	/**
	 * @param triggers
	 * 
	 */
	public void disableTriggers(List<Trigger> triggers)
	{
		for(Trigger trigger : triggers)
			disableTrigger(trigger);
	}
	
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
	
	public void fireTrigger(Trigger trigger)
	{
		if(trigger.getJump() == null)
			return;
		addLogLine("TRIGGER", "Fired, jumping to: " + trigger.getJump().getID());
		goTo(new FieldWithArguments(trigger.getJump(), trigger.getNextFieldArguments()));
	}

	public boolean isFieldEndabled(Field field)
	{
		return field.isEnabled() && !currFormSession.tempDisabledFields.contains(field);
	}
	
	protected void exit()
	{
		// cancel (timer) triggers:
		if(currFormSession.form != null)
			disableTriggers(currFormSession.form.getTriggers());
		
		// stop GPS!
		stopLocationListener();
		
		// Close log file:
		if(logger != null)
		{
			logger.addFinalLine("EXIT_COLLECTOR", project.getName(), currFormSession.form.getID());
			logger = null;
		}
		
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
	public FormSession.Mode getCurrentFormMode()
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
	
	public boolean canGoBack(boolean withinFormOnly)
	{
		return (currFormSession != null && !currFormSession.fieldAndArgumentHistory.empty()) || (!withinFormOnly && !formHistory.empty());
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
	
	protected void startLocationListener(LocationField locField)
	{
		startLocationListener(Arrays.asList(locField));
	}

	public abstract void startLocationListener(List<LocationField> locFields);

	public abstract void stopLocationListener();
	
	public abstract Location getCurrentBestLocation();
	
	public void addMediaAttachment(File mediaAttachment)
	{
		currFormSession.mediaAttachments.add(mediaAttachment);
	}
	
	protected abstract void vibrate(int durationMS);
	
	protected abstract void playSound(File soundFile);
	
	protected abstract void showError(String errorMsg, boolean exit);
	
	protected abstract void exitApp();
	
	public static class FieldWithArguments
	{
		
		protected Field field;
		protected FieldParameters arguments;
		
		/**
		 * @param field the field
		 */
		public FieldWithArguments(Field field)
		{
			this(field, FieldParameters.EMPTY);
		}
		
		/**
		 * @param field the field
		 * @param arguments arguments passed along by previous field
		 */
		public FieldWithArguments(Field field, FieldParameters arguments)
		{
			this.field = field;
			this.arguments = new FieldParameters(arguments); // create a copy!
		}
		
	}
	
	/**
	 * Helper class which holds all state variables needed to manage an open "form session"
	 * 
	 * @author mstevens
	 */
	public static class FormSession
	{

		public static enum Mode
		{
			CREATE,
			EDIT,
			//SELECT
		}
		
		static public FormSession Create(Form form, long deviceIDHash)
		{
			return new FormSession(form, Mode.CREATE, form.isProducesRecords() ? form.newRecord(deviceIDHash) : null);
		}
		
		static public FormSession Edit(Form form, Record record)
		{
			return new FormSession(form, Mode.EDIT, record);
		}
		
		//Dynamic
		protected Form form;
		protected Mode mode;
		protected Record record;
		protected Stack<FieldWithArguments> fieldAndArgumentHistory;
		protected FieldWithArguments currFieldAndArguments = null;
		protected boolean currFieldDisplayed = false;
		protected Set<Field> tempDisabledFields;
		protected List<File> mediaAttachments;	
		protected long startTime;
		
		/**
		 * @param form
		 * @param mode
		 * @param record
		 */
		private FormSession(Form form, Mode mode, Record record)
		{
			if(form == null)
				throw new NullPointerException("Form cannot be null!");
			if(record == null && form.isProducesRecords())
				throw new NullPointerException("Record cannot be null because this is a record-producing form!");	
			this.form = form;
			this.mode = mode;
			this.record = record;
			this.fieldAndArgumentHistory = new Stack<FieldWithArguments>();
			this.tempDisabledFields = new HashSet<Field>();
			this.mediaAttachments = new ArrayList<File>();
			this.startTime = System.currentTimeMillis();
		}
		
		public Field getPreviousField()
		{
			return fieldAndArgumentHistory.isEmpty() ? null : fieldAndArgumentHistory.peek().field;
		}
		
		public Field getCurrentField()
		{
			return currFieldAndArguments != null ? currFieldAndArguments.field : null;
		}
		
		public FieldParameters getCurrentFieldArguments()
		{
			return currFieldAndArguments != null ? currFieldAndArguments.arguments : null;
		}
		
		public boolean atField()
		{
			return currFieldAndArguments != null;
		}
		
	}
	
}
