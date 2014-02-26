package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
import uk.ac.ucl.excites.sapelli.collector.project.data.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.project.ui.ControlsState;
import uk.ac.ucl.excites.sapelli.util.Logger;
import uk.ac.ucl.excites.sapelli.util.io.FileHelpers;

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
	protected DataAccess dao;
	protected Logger logger;
	
	protected long deviceIDHash; //to be initialised by subclasses
	
	protected Stack<FormSession> formHistory;
	protected FormSession currFormSession;
	protected FormSession prevFormSession; 
	
	public Controller(Project project, DataAccess dao)
	{
		this.project = project;
		this.dao = dao;
		
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
		currFormSession = null;
		formHistory.clear();
		
		// Open a Create-mode session for the startForm:
		openFormSession(FormSession.Create(project.getStartForm(), deviceIDHash));
	}

	public void openFormSession(FormSession formSession)
	{
		prevFormSession = currFormSession; // remember previous formSession
		currFormSession = formSession;
		if(!formHistory.isEmpty() && formHistory.peek() != currFormSession) // add to history unless still in the same session
			formHistory.add(currFormSession);
		
		// Location...
		List<LocationField> lfStartWithForm = currFormSession.form.getLocationFields(true);
		if(!lfStartWithForm.isEmpty())
			startLocationListener(lfStartWithForm); // start listening for location updates
		else
			stopLocationListener(); // stop listening for location updates (if we were still listening for another form for example)
	
		// Log start form
		if(logger != null)			
			logger.addLine("FORM_START", currFormSession.form.getName() + " (index: " + currFormSession.form.getIndex() + ")");
		
		// Go to field...
		if(currFormSession.currField == null)
			goTo(currFormSession.form.getStartField()); // begin filling out the form at the start field
		else
			goTo(currFormSession.currField); // continue where we left off
	}

	public void cancelAndRestartForm()
	{
		// Cancel button pressed
		if(logger != null)
			logger.addLine("CANCEL_BUTTON", currFormSession.currField.getID());
		
		goTo(new EndField(currFormSession.form, false, Next.LOOPFORM));
	}

	public void cancelAndStop()
	{
		goTo(new EndField(currFormSession.form, false, Next.EXITAPP));
	}

	public void goForward(boolean requestedByUser)
	{
		// log interaction:
		if(requestedByUser && logger != null)
			logger.addLine("FORWARD_BUTTON", currFormSession.currField.getID());
	
		if(currFormSession.currField != null)
			goTo(currFormSession.form.getNextField(currFormSession.currField));
		else
			openFormSession(currFormSession); // this shouldn't happen really...
	}
	
	/**
	 * @return the current ButtonState
	 */
	public ControlsState getControlsState()
	{
		ControlsState state = new ControlsState(
				currFormSession.form.isShowBack()		&& currFormSession.currField.isShowBack()		&& !currFormSession.fieldHistory.empty(),
				currFormSession.form.isShowCancel()		&& currFormSession.currField.isShowCancel()		&& (!currFormSession.fieldHistory.empty() || currFormSession.currField instanceof Page),
				currFormSession.form.isShowForward()	&& currFormSession.currField.isShowForward()	&& currFormSession.currField.getOptional() == Optionalness.ALWAYS);
		// Note: these paths may be null (in which case built-in defaults must be used)
		return state;
	}

	public void goBack()
	{
		if(!currFormSession.fieldHistory.isEmpty())
		{
			// log interaction:
			if(logger != null)
				logger.addLine("BACK_BUTTON", currFormSession.currField.getID());
	
			currFormSession.currField = null; // !!! otherwise we create loops
			final Field previousField = currFormSession.fieldHistory.pop();
	
			// TODO Maybe there is a better way of handling back buttons (TODO yes there is!)
			if(previousField instanceof LocationField)
				goTo(currFormSession.fieldHistory.pop()); // Move two fields backwards
			else if(currFormSession.currField instanceof OrientationField)
				goTo(currFormSession.fieldHistory.pop()); // Move two fields backwards
			else
				goTo(previousField);
		}
	}

	public synchronized void goTo(Field nextField)
	{
		// log interaction
		if(logger != null)
			logger.addLine("REACHED", nextField.getID());
	
		// Leaving current field...
		if(currFormSession.currField != null && currFormSession.currField != nextField)
			currFormSession.fieldHistory.add(currFormSession.currField); // Add to history
		// Entering next field...
		currFormSession.currField = nextField;
		
		// Enter field and ...
		if(currFormSession.currField.enter(this))
			displayField(currFormSession.currField);
	}

	protected void saveRecordAndAttachments()
	{
		if(!currFormSession.form.isProducesRecords()) //!!!
			return;
		
		// Finalise the currentRecord:
		currFormSession.form.finish(currFormSession.record); // sets end-time if necessary
	
		// Store currentRecord
		dao.store(currFormSession.record);
	
		// Log record:
		if(logger != null)
			logger.addLine("RECORD", currFormSession.record.toString());
	
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
	
	protected void discardAttachments()
	{
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
		// Deal with leaves (should never happen, but just in case...):
		if(cf.isLeaf())
			throw new IllegalStateException("Cannot enter a leaf choice (" + cf.toString() + ")");
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
			goForward(false); // skip field
			return false;
		}
		return true;
	}
	
	/**
	 * @param pf  the PhotoField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterPhotoField(PhotoField pf);
	
	/**
	 * @param lf  the LocationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public abstract boolean enterLocationField(LocationField lf);
	
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
		// TODO does this work?
		//for(Field f : page.getFields())
		//	f.enter(this);
		
		//TODO startWithPage location
		
		return true;
	}
	
	public boolean enterLinksTo(Relationship rel)
	{
	
		
		return false;
	}
	
	public boolean enterBelongsTo(Relationship rel)
	{
		CollectorRecord foreignRecord = null;
		
		// Try to obtain foreign record from previous form:
		if(prevFormSession != null)
		{
			if(prevFormSession.form == rel.getRelatedForm()) // we have come back from the related form
				foreignRecord = prevFormSession.record;
			else if(rel.isHoldForeignRecord() && prevFormSession.form == currFormSession.form) // we have looped within the same form & we are allowed to hold on to the ref
			{
				//foreignRecord = dao.retrieveRecord(/* key taken from prevFormSession.record */);
			}
		}
		// Try to obtain foreign record by database query:
		if(rel.isHoldForeignRecord() && foreignRecord == null)
		{
			//foreignRecord = dao.retrieve(/* last record of related form */);
		}
		
		//TODO Check if the foreignRecord meets the constraints:
		//	make it null if it doesn't
		
		if(foreignRecord != null)
		{	
			//TODO Refer to foreign record from current record
			goForward(false);
		}
		else
			openFormSession(FormSession.Create(rel.getRelatedForm(), deviceIDHash)); ; // Open related from to create a new foreign record
		
		return false;
	}
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef)
	{
		// Logging:
		if(logger != null)
			logger.addLine("FORM_END", ef.getID(), currFormSession.form.getName(), Long.toString((System.currentTimeMillis() - currFormSession.startTime) / 1000) + " seconds");
		
		// Save or discard:
		if(ef.isSave())
			saveRecordAndAttachments();
		else
			discardAttachments();
		
		// Insert blank line in log:
		if(logger != null)
			logger.addBlankLine();
		
		// Go to "next":
		switch(ef.getNext())
		{
			case LOOPFORM:
				formHistory.pop();
				openFormSession(FormSession.Create(currFormSession.form, deviceIDHash));
				break;
			case LOOPPROJ:
				startProject(); // formHistory & currFormSession will be cleared
				break;
			case PREVFORM:
				formHistory.pop();
				if(!formHistory.isEmpty())
					openFormSession(formHistory.pop()); // re-open previous form 
				else
					showError("Invalid state: no previous form to return to!", true); //TODO multilang
				break;
			case EXITAPP:
				exit();
				break;
		}		
		
		return false; // no UI update needed
	}
	
	public boolean isFieldEndabled(Field field)
	{
		return field.isEnabled() && !currFormSession.tempDisabledFields.contains(field);
	}

	public void choiceMade(ChoiceField chosenChild)
	{
		// Note: chosenChild is not the currentField! The currentField (also a ChoiceField) is its parent.
		if(chosenChild.isLeaf())
		{
			// Store value
			if(!chosenChild.getRoot().isNoColumn())
				chosenChild.storeValue(currFormSession.record);
			// Go to next field
			goTo(currFormSession.form.getNextField(chosenChild));
			/*
			 * We cannot use goForward() here because then we would first need to make the chosenChild the currentField, in which case it would end up in the
			 * fieldHistory which does not make sense because a leaf choice cannot be displayed on its own.
			 */
		}
		else
			goTo(chosenChild); // chosenChild becomes the new currentField (we go one level down in the choice tree)
	}

	public void mediaDone(File mediaAttachment)
	{
		MediaField ma = (MediaField) currFormSession.currField;
		if(mediaAttachment != null && mediaAttachment.exists())
		{
			if(logger != null)
				logger.addLine("ATTACHMENT", currFormSession.currField.getID(), mediaAttachment.getName());
			
			ma.incrementCount(currFormSession.record); // Store/increase number of pictures/recordings taken
			if(ma.isMaxReached(currFormSession.record) && ma.getDisableChoice() != null)
				currFormSession.tempDisabledFields.add(ma.getDisableChoice()); // disable the choice that makes the MA accessible
			currFormSession.mediaAttachments.add(mediaAttachment);
			goForward(false); // goto next/jump field
		}
		else
		{
			if(logger != null)
				logger.addLine("ATTACHMENT", currFormSession.currField.getID(), "NONE");
			
			if(ma.getOptional() != Optionalness.ALWAYS)
				// at least one attachment is required:
				goTo(ma); // stay at this field
			else
				goForward(false); // goto next/jump field
		}
	}
	
	protected void exit()
	{
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
	 * @return the currentForm
	 */
	public Form getCurrentForm()
	{
		return currFormSession.form;
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
	 * @return the mode of the currently open form
	 */
	public FormSession.Mode getCurrentFormMode()
	{
		return currFormSession.mode;
	}

	/**
	 * @return the currentField
	 */
	public Field getCurrentField()
	{
		return currFormSession.currField;
	}
	
	protected void startLocationListener(LocationField locField)
	{
		startLocationListener(Arrays.asList(locField));
	}

	public abstract void startLocationListener(List<LocationField> locFields);

	public abstract void stopLocationListener();
	
	protected abstract void displayField(Field currentField);
	
	protected abstract void vibrate(int durationMS);
	
	protected abstract void playSound(File soundFile);
	
	protected abstract void showError(String errorMsg, boolean exit);
	
	protected abstract void exitApp();
	
	/**
	 * Helper class which holds all state variables needed to manage an open "form session"
	 * 
	 * @author mstevens
	 */
	static protected class FormSession
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
		Form form;
		Mode mode;
		CollectorRecord record;
		Stack<Field> fieldHistory;
		Field currField = null;
		Set<Field> tempDisabledFields;
		List<File> mediaAttachments;	
		long startTime;
		
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
		
	}
	
}
