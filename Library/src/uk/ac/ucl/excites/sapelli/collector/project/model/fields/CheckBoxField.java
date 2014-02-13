/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.model.fields;

import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.project.ui.Controller;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author Julia
 * 
 */
public class CheckBoxField extends Field
{

	static public final String ID_PREFIX = "chbx";
	
	static public final boolean DEFAULT_INITIAL_VALUE = false; // not ticked by default
	
	private String label;
	private boolean initialValue;

	/**
	 * @param form
	 * @param id
	 * @parap label
	 */
	public CheckBoxField(Form form, String id, String label)
	{
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + label.trim().replaceAll("\\s+", "_") : id));
		this.label = label;
		this.initialValue = DEFAULT_INITIAL_VALUE;
	}

	/**
	 * @return the label
	 */
	public String getLabel()
	{
		return label;
	}

	/**
	 * @return the initialValue
	 */
	public boolean getInitialValue()
	{
		return initialValue;
	}
	
	/**
	 * @param initialValue the initialValue to set
	 */
	public void setInitialValue(boolean initialValue)
	{
		this.initialValue = initialValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		return new BooleanColumn(id, optional != Optionalness.NEVER);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		return controller.enterCheckBoxField(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createCheckBoxFieldUI(this);
	}

}
