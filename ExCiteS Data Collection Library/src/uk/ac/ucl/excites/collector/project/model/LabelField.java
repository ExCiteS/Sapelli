/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Column;

/**
 * @author mstevens
 *
 */
public class LabelField extends Field
{

	static public final String ID_PREFIX = "lbl";
	
	private String text;
	
	/**
	 * @param form
	 * @param text
	 */
	public LabelField(Form form, String text)
	{
		this(form, null, text);
	}
	
	/**
	 * @param form
	 * @param id
	 * @param text
	 */
	public LabelField(Form form, String id, String text)
	{	
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + text.trim().replaceAll("\\s+","_") : id));
		this.text = text;
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
	 * @return the text
	 */
	public String getText()
	{
		return text;
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
		throw new UnsupportedOperationException("setNoColumn is unsupported on LabelFields since they never have columns.");
	}

}