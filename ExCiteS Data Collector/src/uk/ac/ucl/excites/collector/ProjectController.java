/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.util.Stack;

import uk.ac.ucl.excites.collector.model.Choice;
import uk.ac.ucl.excites.collector.model.Field;
import uk.ac.ucl.excites.collector.model.Form;
import uk.ac.ucl.excites.collector.model.Project;
import uk.ac.ucl.excites.storage.db.DataStorageAccess;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class ProjectController
{
	
	private Project project;
	private DataStorageAccess dsa;
	private Form currentForm;
	private Field currentField;
	
	private long deviceID;
	
	private Stack<Field> fieldHistory;
	
	public ProjectController(Project project, DataStorageAccess dsa)
	{
		this.project = project;
		this.dsa = dsa;
		fieldHistory = new Stack<Field>();
		
		//TODO get device ID
		deviceID = 0;
		
		//For now projects have only one form, so set the current form:
		setForm(0);
	}
	
	public void setForm(int index)
	{
		currentForm = project.getForms().get(index);
		
	}
	
	//TODO setForm by name
	
	public void startForm()
	{
		currentField = currentForm.getStart();
		fieldHistory.clear();
		
	}
	
	public void goTo(Field nextField)
	{
		//Save value if necessary
		// Choices
		if(currentField instanceof Choice)
		{
			Choice currentChoice = (Choice) currentField;
			//if(!(nextField instanceof Choice) || ((Choice) nextField).getRoot() != currentChoice.getRoot())
				//currentChoice.getRoot()
		}
		// Locations
		//...
		// MediaAttachments
		// ...
		
	}
	
	public void goBack()
	{
		if(!fieldHistory.isEmpty())
		{
			//goTo(fieldHistory.pop());
		}
	}
	
	public void endForm()
	{
		//Store values
		Schema schema = currentForm.getSchema(dsa);
		Record record = new Record(schema, deviceID);
		for(Field f : currentForm.getFields())
			f.storeValues(record);
		//End action:
		
	}
		
}
