/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
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
	public boolean enter(Controller controller)
	{
		return controller.enterCancelField(this);
	}

	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return null; // there is not UI for this field
	}

	@Override
	public void leave(FieldUI ui, Controller controller)
	{
		// TODO Auto-generated method stub
		
	}

}
