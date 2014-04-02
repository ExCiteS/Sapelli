/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LabelUI;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author mstevens
 *
 */
public class LabelField extends Field
{

	static public final String ID_PREFIX = "lbl";
	static public final float DEFAULT_TEXT_SIZE_SCALE = 1.0f; // same as surrounding text  
	static public final boolean DEFAULT_TEXT_CENTERED = false;
	
	private float textSizeScale = DEFAULT_TEXT_SIZE_SCALE;
	private boolean centered = DEFAULT_TEXT_CENTERED;
	
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
	 * @param labelText
	 */
	public LabelField(Form form, String id, String labelText)
	{	
		super(form, (id == null || id.isEmpty() ? ID_PREFIX + (labelText.trim().isEmpty() ? form.getFields().size() : StringUtils.replaceWhitespace(labelText.trim(), "_")) : id), labelText);
		this.noColumn = true;
	}
	
	/**
	 * @return the textSizeScale
	 */
	public float getTextSizeScale()
	{
		return textSizeScale;
	}

	/**
	 * @param textSizeScale the textSizeScale to set
	 */
	public void setTextSizeScale(float textSizeScale)
	{
		this.textSizeScale = textSizeScale;
	}

	/**
	 * @return the centered
	 */
	public boolean isCentered()
	{
		return centered;
	}

	/**
	 * @param centered the centered to set
	 */
	public void setCentered(boolean centered)
	{
		this.centered = centered;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, boolean onPage)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V> LabelUI<V> createUI(CollectorUI<V> collectorUI)
	{
		return collectorUI.createLabelUI(this);
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn is unsupported on LabelFields since they never have columns.");
	}
	
}