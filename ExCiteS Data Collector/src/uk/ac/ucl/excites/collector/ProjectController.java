/**
 * 
 */
package uk.ac.ucl.excites.collector;

import uk.ac.ucl.excites.collector.model.Field;
import uk.ac.ucl.excites.collector.model.Form;
import uk.ac.ucl.excites.collector.model.Project;
import uk.ac.ucl.excites.storage.db.DataStorageAccess;

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
	
	public ProjectController(Project project, DataStorageAccess dsa)
	{
		this.project = project;
		this.dsa = dsa;
		
		//For now projects have only one form, so set the current form:
		setForm(0);
	}
	
	public void setForm(int index)
	{
		currentForm = project.getForms().get(index);
		currentField = currentForm.getStart();
	}
	
	//TODO setForm by name
	
	
	
}
