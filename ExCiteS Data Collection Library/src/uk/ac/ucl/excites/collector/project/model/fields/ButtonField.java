/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.util.StringUtils;

/**
 * @author mstevens
 *
 */
public class ButtonField extends Field
{

	static public final String ID_PREFIX = "btn";
	
	private String label;
	
	/**
	 * @param form
	 * @param label
	 */
	public ButtonField(Form form, String label)
	{
		this(form, null, label);
	}
	
	/**
	 * @param form
	 * @param id
	 * @param label
	 */
	public ButtonField(Form form, String id, String label)
	{	
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + StringUtils.replaceWhitespace(label.trim(), "_") : id));
		this.label = label;
		this.noColumn = true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		return null;
	}
	
	/**
	 * @return the label
	 */
	public String getLabel()
	{
		return label;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites.collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		// TODO Auto-generated method stub
		return null;
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn is unsupported on ButtonFields since they never have columns.");
	}

	@Override
	public void leave(FieldUI ui, Controller controller)
	{
		// TODO Auto-generated method stub
		
	}

}
