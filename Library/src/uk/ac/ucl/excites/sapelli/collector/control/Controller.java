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
import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.lists.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.project.ui.ControlsState;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.util.Logger;
import uk.ac.ucl.excites.sapelli.util.io.FileHelpers;

public abstract class Controller
{
	
	private static final String LOG_PREFIX = "Collector_";
	public static final int VIBRATION_DURATION_MS = 600;
	
	protected Project project;
	protected DataAccess dao;
	protected long deviceIDHash;
	protected Stack<FormSession> formHistory;
	private FormSession currentFormSession;
	protected Form currentForm;
	protected Field currentField;
	protected Set<Field> tempDisabledFields;
	protected Stack<Field> fieldHistory;
	protected Record currentRecord;
	protected List<File> currentMediaAttachments;
	protected long formStartTime;
	protected Logger logger;
	
	public Controller(Project project, DataAccess dao)
	{
		this.project = project;
		this.dao = dao;
		
		// Collections:
		formHistory = new Stack<FormSession>();
		fieldHistory = new Stack<Field>();
		tempDisabledFields = new HashSet<Field>();
		currentMediaAttachments = new ArrayList<File>();	
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
		startForm(project.getStartForm()); // start with startForm (which is the first parsed form by default)
	}

	public void startForm(String formName)
	{
		// Find form with the given name:
		Form form = null;
		for(Form f : project.getForms())
			if(f.getName().equals(formName))
			{
				form = f;
				break;
			}
		if(form != null)
			startForm(form);
		else
			throw new IllegalArgumentException("Form " + formName + " could not be found in this project.");
	}

	public void startForm(int formIndex)
	{
		startForm(project.getForms().get(formIndex));
	}

	public void startForm(Form form)
	{
		currentForm = form;
	
		// Clear stuff:
		fieldHistory.clear();
		tempDisabledFields.clear();
		currentMediaAttachments.clear();
		currentField = null;
	
		// Create new currentRecord:
		currentRecord = currentForm.newEntry(deviceIDHash);
		
		// Location...
		List<LocationField> lfStartWithForm = currentForm.getLocationFields(true);
		if(!lfStartWithForm.isEmpty())
			startLocationListener(lfStartWithForm); // start listening for location updates
		else
			stopLocationListener(); // stop listening for location updates (if we were still listening for another form for example)
	
		// log start form
		if(logger != null)
		{
			formStartTime = System.currentTimeMillis();
			logger.addLine("FORM_START", currentForm.getName() + " (index: " + currentForm.getIndex() + ")");
		}
		
		// Begin filling out the form at the start field:
		goTo(currentForm.getStartField());
	}

	public void cancelAndRestartForm()
	{
		// Cancel button pressed
		if(logger != null)
			logger.addLine("CANCEL_BUTTON", currentField.getID());
		
		goTo(new EndField(currentForm, false, Next.LOOPFORM));
	}

	public void cancelAndStop()
	{
		goTo(new EndField(currentForm, false, Next.EXITAPP));
	}

	public void goForward(boolean requestedByUser)
	{
		// log interaction:
		if(requestedByUser && logger != null)
			logger.addLine("FORWARD_BUTTON", currentField.getID());
	
		if(currentField != null)
			goTo(currentForm.getNextField(currentField));
		else
			startForm(currentForm); // this shouldn't happen really...
	}
	
	/**
	 * @return the current ButtonState
	 */
	public ControlsState getControlsState()
	{
		ControlsState state = new ControlsState(
				currentForm.isShowBack()	&& currentField.isShowBack()	&& !fieldHistory.empty(),
				currentForm.isShowCancel()	&& currentField.isShowCancel()	&& (!fieldHistory.empty() || currentField instanceof Page),
				currentForm.isShowForward()	&& currentField.isShowForward()	&& currentField.getOptional() == Optionalness.ALWAYS);
		// Note: these paths may be null (in which case built-in defaults must be used)
		return state;
	}

	public void goBack()
	{
		if(!fieldHistory.isEmpty())
		{
			// log interaction:
			if(logger != null)
				logger.addLine("BACK_BUTTON", currentField.getID());
	
			currentField = null; // !!! otherwise we create loops
			final Field previousField = fieldHistory.pop();
	
			// TODO Maybe there is a better way of handling back buttons
			if(previousField instanceof LocationField)
				goTo(fieldHistory.pop()); // Move two fields backwards
			else if(currentField instanceof OrientationField)
				goTo(fieldHistory.pop()); // Move two fields backwards
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
		if(currentField != null && currentField != nextField)
			fieldHistory.add(currentField); // Add to history
		// Entering next field...
		currentField = nextField;
		
		// Enter field and ...
		if(currentField.enter(this))
			displayField(currentField);
	}

	protected void saveRecordAndAttachments()
	{
		if(!currentForm.isProducesRecords()) //!!!
			return;
		
		// Finalise the currentRecord:
		currentForm.finish(currentRecord); // sets end-time if necessary
	
		// Store currentRecord
		dao.store(currentRecord);
	
		// Log record:
		if(logger != null)
			logger.addLine("RECORD", currentRecord.toString());
	
		// Move attachments from temp to data folder:
		try
		{
			File dataFolder = project.getDataFolder();
			for(File attachment : currentMediaAttachments)
				attachment.renameTo(new File(dataFolder.getAbsolutePath() + File.separator + attachment.getName()));
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace(System.err);
		}
	
		// Signal the successful storage of the currentRecord
		// Vibration
		if(currentForm.isVibrateOnSave())
			vibrate(VIBRATION_DURATION_MS);
		// Play sound
		File endSoundFile = project.getSoundFile(currentForm.getSaveSoundRelativePath());
		if(FileHelpers.isReadableFile(endSoundFile))
			playSound(endSoundFile);		
	}
	
	protected void discardAttachments()
	{
		// Delete any attachments:
		for(File attachment : currentMediaAttachments)
			if(attachment.exists())
				attachment.delete();
		currentMediaAttachments.clear();
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
		if(mf.isMaxReached(currentRecord))
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
	 * @param page	the EditTextField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterTextField(EditTextField tf)
	{
		return true;
	}
	
	/**
	 * @param page	the CheckBoxField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterCheckBoxField(CheckBoxField cbf)
	{	
		return true;
	}
	
	/**
	 * @param page	the ButtonField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterButtonField(ButtonField bf)
	{
		return true;
	}
	
	/**
	 * @param page	the MultiListField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMultiListField(MultiListField mlf)
	{
		return true;
	}
	
	/**
	 * @param page	the Page
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterPage(Page page)
	{
		// TODO does this work?
		//for(Field f : page.getFields())
		//	f.enter(this);
		return true;
	}
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef)
	{
		// Logging:
		if(logger != null)
			logger.addLine("FORM_END", ef.getID(), currentForm.getName(), Long.toString((System.currentTimeMillis() - formStartTime) / 1000) + " seconds");
		
		if(ef.isSave())
			saveRecordAndAttachments();
		else
			discardAttachments();
		
		// Insert blank line in log:
		if(logger != null)
			logger.addBlankLine();
		
		// Next action:
		switch(ef.getNext())
		{
			case LOOPFORM:
				startForm(currentForm);
				break;
			case LOOPPROJ:
				//TODO LOOPPROJ implementation
				break;
			case PREVFORM:
				//TODO PREVFORM implementation
				break;
			case EXITAPP:
				exit();
				break;
		}		
		
		return false; // no UI update needed
	}
	
	public boolean isFieldEndabled(Field field)
	{
		return field.isEnabled() && !tempDisabledFields.contains(field);
	}

	public void choiceMade(ChoiceField chosenChild)
	{
		// Note: chosenChild is not the currentField! The currentField (also a ChoiceField) is its parent.
		if(chosenChild.isLeaf())
		{
			// Store value
			if(!chosenChild.getRoot().isNoColumn())
				chosenChild.storeValue(currentRecord);
			// Go to next field
			goTo(currentForm.getNextField(chosenChild));
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
		MediaField ma = (MediaField) currentField;
		if(mediaAttachment != null && mediaAttachment.exists())
		{
			if(logger != null)
				logger.addLine("ATTACHMENT", currentField.getID(), mediaAttachment.getName());
			
			ma.incrementCount(currentRecord); // Store/increase number of pictures/recordings taken
			if(ma.isMaxReached(currentRecord) && ma.getDisableChoice() != null)
				tempDisabledFields.add(ma.getDisableChoice()); // disable the choice that makes the MA accessible
			currentMediaAttachments.add(mediaAttachment);
			goForward(false); // goto next/jump field
		}
		else
		{
			if(logger != null)
				logger.addLine("ATTACHMENT", currentField.getID(), "NONE");
			
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
			logger.addFinalLine("EXIT_COLLECTOR", project.getName(), currentForm.getID());
			logger = null;
		}
		
		exitApp();
	}

	/**
	 * @return the currentForm
	 */
	public Form getCurrentForm()
	{
		return currentForm;
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
		return currentRecord;
	}

	/**
	 * @return the currentField
	 */
	public Field getCurrentField()
	{
		return currentField;
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
	
	protected abstract void exitApp();
	
}
