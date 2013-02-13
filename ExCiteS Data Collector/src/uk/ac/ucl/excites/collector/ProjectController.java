/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.util.Stack;

import android.location.Location;
import android.location.LocationListener;
import android.nfc.FormatException;
import android.os.Bundle;

import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.FormEntry;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class ProjectController implements LocationListener
{
	
	private Project project;
	private DataAccess dao;
	private Form currentForm;
	private FormEntry entry;
	private Field currentField;
	private CollectorActivity activity;
	
	private long deviceID;
	
	private Stack<Field> fieldHistory;
	
	public ProjectController(Project project, DataAccess dao, CollectorActivity activity)
	{
		this.project = project;
		this.dao = dao;
		this.activity = activity;
		
		fieldHistory = new Stack<Field>();
		
		//TODO get device ID
		deviceID = 0;
		
		startForm(0); //For now projects have only one form
	}
	
	public void startForm(String formName)
	{
		//Find form with the given name:
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
		restartForm();
	}
	
	public void restartForm()
	{
		fieldHistory.clear();
		
		entry = currentForm.newEntry(dao); //pass deviceID
		
		currentField = null;
		goTo(currentForm.getStart());
	}
	
	public void goForward()
	{
		if(currentField != null)
			goTo(currentForm.getNextField(currentField));
		else
			restartForm(); //this shouldn't happen
	}
	
	public void goBack()
	{
		if(!fieldHistory.isEmpty())
			goTo(fieldHistory.pop());
	}
	
	public void goTo(Field nextField)
	{
		//Leafing current field...
		if(currentField != null)
			fieldHistory.add(currentField); //Add to history
		//Entering next field...
		currentField = nextField;
		// Choices
		if(currentField instanceof Choice)
		{
			//TODO set ChoiceView in activity
		}
		// Location
		else if(currentField instanceof LocationField)
		{
			//TODO
		}
		// MediaAttachment
		
		// ...
		// _END
		if(currentField.equals(EndField.getInstance()))
			endForm();
	}
	
	/**
	 * To be called from UI/CollectorActivity
	 * 
	 * @param chosenChild
	 */
	public void choiceMade(Choice chosenChild)
	{
		//Note: chosenChild is not the currentField! The currentField (also a Choice) is its parent. 
		if(chosenChild.isLeaf())
		{
			//Store value
			if(!chosenChild.getRoot().isNoColumn())
				chosenChild.storeValue(entry);
			//Go to next field
			goTo(currentForm.getNextField(chosenChild));
			/* We cannot use goForward() here because then we would first need to make the
			 * chosenChild the currentField, in which case it would end up in the fieldHistory
			 * which does not make sense because a leaf choice cannot be displayed on its own.
			 */
		}
		else
			goTo(chosenChild); //chosenChild becames the new currentField (we go one level down in the choice tree)
	}
	
	public void photoDone(boolean pictureTaken)
	{
		
	}
	
	public void audioDone(boolean recordingMade)
	{
		
	}
	
	public void endForm()
	{
		//Store entry
		entry.store(); //saves entry in database
		//End action:
		switch(currentForm.getEndAction())
		{
			case Form.END_ACTION_LOOP : restartForm(); break;
			case Form.END_ACTION_EXIT : activity.finish(); break; //leaves the application!
		}
	}

	@Override
	public void onLocationChanged(Location location)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onProviderDisabled(String provider)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onProviderEnabled(String provider)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onStatusChanged(String provider, int status, Bundle extras)
	{
		// TODO Auto-generated method stub
		
	}
		
}
