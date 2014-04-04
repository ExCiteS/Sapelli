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
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsState;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
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
	protected CollectorUI<?> ui;
	protected RecordStore recordStore;
	protected Logger logger;
	
	protected long deviceIDHash; //to be initialised by subclasses
	
	protected Stack<FormSession> formHistory;
	protected FormSession currFormSession;
	protected FormSession prevFormSession; 
	
	protected boolean handlingUserGoBackRequest = false;
	
	public Controller(Project project, CollectorUI<?> ui, RecordStore recordStore)
	{
		this.project = project;
		this.ui = ui;
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
	
	public void openFormSession(FormSession formSession, boolean prevFormMove)
	{
		// Deal with current form session:
		if(currFormSession != null)
		{
			prevFormSession = currFormSession; // remember previous formSession
			if(	!prevFormMove &&							// If we are not "coming back",
				currFormSession.form != formSession.form && // AND we are not looping within the same form,
				!currFormSession.form.isSkipOnBack())		// AND the previous form does not have skipOnBack=true,
				formHistory.push(currFormSession);			// ... add previous formSession to history
			disableTriggers(currFormSession.form.getTriggers()); // disable triggers
		}
		currFormSession = formSession;
				
		// Location...
		List<LocationField> lfStartWithForm = currFormSession.form.getLocationFields(true);
		if(!lfStartWithForm.isEmpty())
			startLocationListener(lfStartWithForm); // start listening for location updates
		else
			stopLocationListener(); // stop listening for location updates (if we were still listening for another form for example)
	
		// Log start form
		addLogLine("FORM_START", currFormSession.form.getName() + " (index: " + currFormSession.form.getPosition() + ")");
		
		// Setup the triggers
		setupTriggers(currFormSession.form.getTriggers());

		// Go to field...
		if(currFormSession.currField == null)
			goTo(currFormSession.form.getStartField()); // begin filling out the form at the start field
		else
			goTo(currFormSession.currField); // continue where we left off
	}
	
	public void cancelAndRestartForm()
	{
		// Cancel button pressed
		addLogLine("CANCEL_BUTTON", currFormSession.currField.getID());
		
		goTo(new EndField(currFormSession.form, false, Next.LOOPFORM), true); // loop without saving first (forced leaving of current field)
	}

	public void cancelAndStop()
	{
		goTo(new EndField(currFormSession.form, false, Next.EXITAPP), true); // exit without saving first (forced leaving of current field)
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
		
		// Log interaction:
		if(requestedByUser)
			addLogLine("FORWARD_BUTTON", currFormSession.currField.getID());
		
		if(currFormSession.currField != null)
			goTo(currFormSession.form.getNextField(currFormSession.currField));
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
		{	// Remember we are handling a user initiated goBack request, this will turn subsequently triggered automatic goForward requests into goBack requests!
			handlingUserGoBackRequest = true; // Do *not* replace this by: handlingGoBackRequest = requestedByUser
		
			// log interaction:
			addLogLine("BACK_BUTTON", currFormSession.currField.getID());
		}
		
		// Try to go to previous field...
		if(!currFormSession.fieldHistory.isEmpty())
		{
			currFormSession.currField = null; // !!! otherwise we create loops
			currFormSession.currFieldDisplayed = false;
			goTo(currFormSession.fieldHistory.pop());
		}
		else
			// Try to go to previous form...
			goToPreviousForm();
		
		// Reset user go back request flag:
		if(requestedByUser)
			handlingUserGoBackRequest = false;
	}
	
	public synchronized void goTo(Field nextField)
	{
		goTo(nextField, false);
	}
	
	/**
	 * @param nextField
	 * @param forceLeave when true the previous field is left without any validation nor storage(!), under the assumption the user will come back there. It is *not* the same as the noValidation argument on leave(Record,boolean).
	 */
	public synchronized void goTo(Field nextField, boolean forceLeave)
	{
		// Null check...
		if(nextField == null)
		{	
			addLogLine("NULL_FIELD");
			return;
		}
	
		// Deal with current field...
		if(currFormSession.currField != null)
		{
			// Check if we are allowed to leave the current field...	
			if(!forceLeave && currFormSession.currFieldDisplayed && !ui.getCurrentFieldUI().leave(currFormSession.record)) // check if we are allowed to leave the currently displayed field (unless the leaving is forced)
			{
				addLogLine("STAY", "Not allowed to leave field " + currFormSession.currField.getID());
				return; // not allowed to leave
			}
			
			// Add current field to history if it is not the same as the next field & it is not to be skipped upon back...
			if(currFormSession.currField != nextField && !currFormSession.currField.isSkipOnBack())
				currFormSession.fieldHistory.push(currFormSession.currField);
		}
		
		// 	Next field becomes the (new) current field...
		currFormSession.currField = nextField;
		
		// Skip the new current field if it is not meant to be shown on create/edit...
		if(	(currFormSession.mode == Mode.CREATE && !currFormSession.currField.isShowOnCreate()) ||
			(currFormSession.mode == Mode.EDIT && !currFormSession.currField.isShowOnEdit()))
		{
			addLogLine("SKIPPING", currFormSession.currField.getID());
			goForward(false);
			return;
		}
		
		// Entering new current field...
		addLogLine("REACHED", currFormSession.currField.getID());
		Field holdCurrentField = currFormSession.currField; // remember the "current" current field, to deal with case in which another goForward/goTo call happens from the enter() method (e.g. when entering a leaf choice)
		boolean needsUIUpdate = currFormSession.currField.enter(this, false);
		// If the current field has changed as part of the entering we are done here: 
		if(currFormSession.currField != holdCurrentField)
			return;
		//else: update UI if needed and remember whether current field is displayed
		if(needsUIUpdate)
			ui.setField(currFormSession.currField);
		currFormSession.currFieldDisplayed = needsUIUpdate;
	}

	/**
	 * @return the current ButtonState
	 */
	public ControlsState getControlsState()
	{
		ControlsState state = new ControlsState(
				currFormSession.currField.isShowBack()		&& (!currFormSession.fieldHistory.empty() || !formHistory.empty()),
				currFormSession.currField.isShowCancel()	&& (!currFormSession.fieldHistory.empty() || currFormSession.currField instanceof Page),
				currFormSession.currField.isShowForward()	&& currFormSession.currField.getOptional() == Optionalness.ALWAYS);
		// Note: these paths may be null (in which case built-in defaults must be used)
		
		
		/* TODO optional/valid logic: 
		 * 
		 * (optionalness=always && (field.isNoColumn() || !field.getcolumn.isvalueset(record))) || (optionalness!=always && fieldUI.isValid()))
		 * 		 * 	
		 * assumption: a set value is (still?) valid (is this true for locations?)
		 * 
		 * Will this work for pages?
		 */
		
		return state;
	}
	
	protected void saveRecordAndAttachments()
	{
		if(!currFormSession.form.isProducesRecords()) //!!!
			return;
		
		// Finalise the currentRecord:
		currFormSession.form.finish(currFormSession.record); // (re)sets the end-time if necessary
	
		// Store currentRecord
		recordStore.store(new Record(currFormSession.record)); //TODO remove new Record()  (this is a hopefully temporarily hack to deal with db4o problems with CollectorRecords for Forms that have Pages)
	
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
	public boolean enterChoiceField(ChoiceField cf)
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
	public boolean enterMediaField(MediaField mf)
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
	public boolean enterLocationField(LocationField lf, boolean withPage)
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
	public abstract boolean enterOrientationField(OrientationField of);
	
	/**
	 * @param page	the Page
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterPage(Page page)
	{
		// Deal with returning from field that is part of the page itself:
		Field prevField = currFormSession.getPreviousField();
		while(prevField != null && (prevField == page || page.getFields().contains(prevField.getRoot())))
		{
			currFormSession.fieldHistory.pop();
			prevField = currFormSession.getPreviousField();
		}
		
		// Enter child fields (but signal that they are entered as part of entering the page):
		for(Field f : page.getFields())
		{	
			if(	(currFormSession.mode == Mode.CREATE && !f.isShowOnCreate()) ||
				(currFormSession.mode == Mode.EDIT && !f.isShowOnEdit()))
				addLogLine("SKIPPING", currFormSession.currField.getID());
			else
				f.enter(this, true); // enter with page
		}
		
		// Setup the triggers
		setupTriggers(page.getTriggers());

		return true;
	}
	
	public boolean enterLinksTo(Relationship rel)
	{
		CollectorRecord foreignRecord = getHeldRecord(rel);
		if(foreignRecord != null)
			openFormSession(FormSession.Edit(rel.getRelatedForm(), foreignRecord)); // Edit the "held" record
		else
			openFormSession(FormSession.Create(rel.getRelatedForm(), deviceIDHash)); ; // Open related from to create a new record
		return false;
	}
	
	public boolean enterBelongsTo(Relationship rel)
	{
		// Check if we already have a foreign key value...
		if(rel.getColumn().isValueSet(currFormSession.record))
			goForward(false);
		
		// TODO find a way to open existing foreign record for editing (+ coming back without going in some kind of loop)
		
		CollectorRecord foreignRecord = null;
		
		// Try to obtain foreignRecord in various ways...
		if(prevFormSession != null && prevFormSession.form == rel.getRelatedForm())
		{	// We have come back from the related form ...
			if(prevFormSession.record == null && rel.getOptional() == Optionalness.ALWAYS) // ... but without a saved record, this is OK only if the belongTo field is optional
				goForward(true); // go to next field (no foreign key stored)
			else if(rel.getConstraints().isValid(prevFormSession.record)) // ... we'll use the record if it meets requirements (if it's null, isValid() will return false)
				foreignRecord = prevFormSession.record;
		}
		else
			foreignRecord = getHeldRecord(rel); // get held record (can be null, for instance if this relationship doesn't hold on to foreign records)
		
		// Deal with foreignRecord and continue...
		if(foreignRecord != null)
		{
			// Store foreign key:
			rel.getColumn().storeValue(currFormSession.record, new ForeignKey(foreignRecord));
			
			// Go to next field in current form...
			goForward(true);
		}
		else
		{	// Open related from to create a new foreign record
			openFormSession(FormSession.Create(rel.getRelatedForm(), deviceIDHash));
		}
		return false;
	}
	
	private CollectorRecord getHeldRecord(Relationship rel)
	{
		if(!rel.isHoldForeignRecord())
			return null;

		ForeignKeyColumn column = (ForeignKeyColumn) rel.getColumn();
		
		// Try to obtain the "held" foreign record:
		CollectorRecord foreignRecord = null;
		
		// Using the previous form session...
		if(	!rel.isNoColumn() &&							/* If the relationship has a column to store foreign keys (i.e. it is not of type LINK) */
			prevFormSession != null &&						/* AND there is a previous form session */		
			prevFormSession.form == currFormSession.form &&	/* AND it is for the same as the current form (i.e. we have looped within the same form) */
			column.isValueSet(prevFormSession.record))		/* AND the previous record stored a foreign key (if optional it can also be null) */
		{
			ForeignKey key = column.retrieveValue(prevFormSession.record);								// get foreign key from previous record
			foreignRecord = (CollectorRecord) recordStore.retrieveRecord(key.getForeignRecordQuery());	// look-up the corresponding foreign record
		}
		
		// Check if the foreignRecord meets the constraints...
		if(rel.getConstraints().isValid(foreignRecord)) // passing null will return false
			foreignRecord = null; // ... and make it null if it doesn't
		
		// If we still don't have one...
		if(foreignRecord == null)
			// ... then try to obtain it by querying for most recent record that meets the relationship constraints...
			foreignRecord = (CollectorRecord) recordStore.retrieveRecord(rel.getHeldRecordQuery()); // TODO no deviceid (for local/remote source) is checked for now
		
		return foreignRecord; // may still be null!
	}
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef)
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
		
		return false; // no UI update needed
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
		goTo(trigger.getJump());
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
	public CollectorRecord getCurrentRecord()
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
		return currFormSession.currField;
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
		
		static public FormSession Edit(Form form, CollectorRecord record)
		{
			return new FormSession(form, Mode.EDIT, record);
		}
		
		//Dynamic
		protected Form form;
		protected Mode mode;
		protected CollectorRecord record;
		protected Stack<Field> fieldHistory;
		protected Field currField = null;
		protected boolean currFieldDisplayed = false;
		protected Set<Field> tempDisabledFields;
		protected List<File> mediaAttachments;	
		protected long startTime;
		
		/**
		 * @param form
		 * @param mode
		 * @param record
		 */
		private FormSession(Form form, Mode mode, CollectorRecord record)
		{
			this.form = form;
			this.mode = mode;
			this.record = record;
			this.fieldHistory = new Stack<Field>();
			this.tempDisabledFields = new HashSet<Field>();
			this.mediaAttachments = new ArrayList<File>();
			this.startTime = System.currentTimeMillis();
		}
		
		public Field getPreviousField()
		{
			return fieldHistory.isEmpty() ? null : fieldHistory.peek();
		}
		
	}
	
}
