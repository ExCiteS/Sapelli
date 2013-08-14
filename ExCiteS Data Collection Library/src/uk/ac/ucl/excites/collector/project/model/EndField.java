/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.Column;

/**
 * Dummy field to represent the end of any form.
 * 
 * @author mstevens
 */
public class EndField extends Field
{

	static public final String ID = "_END";
	
	static public final String ID(Form form)
	{
		return ID + "_" + form.getName();
	}
	
	public EndField(Form form)
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
		ui.setEndField(this);
	}

	@Override
	public List<File> getFiles(Project project)
	{
		return null;
	}

}
