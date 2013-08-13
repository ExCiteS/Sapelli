/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.Column;

/**
 * Dummy field to represent the cancelling of the current form session - triggering a return to the start field without any data being saved.
 * 
 * @author mstevens
 */
public class CancelField extends Field
{

	static public final String ID = "_CANCEL";
	
	static public final String ID(Form form)
	{
		return ID + "_" + form.getName();
	}
	
	public CancelField(Form form)
	{
		super(form, ID(form));
		noColumn = true;
	}

	@Override
	protected Column<?> createColumn()
	{
		return null;
	}

	@Override
	public void setIn(CollectorUI ui)
	{
		ui.setCancelField(this);
	}

	@Override
	public List<File> getFiles(Project project)
	{
		return null;
	}

}
