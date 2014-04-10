/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LabelUI;
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
	 * @param id
	 * @param caption the label's caption, cannot be null.
	 */
	public LabelField(Form form, String id, String caption)
	{	
		super(	form,
				(id == null || id.isEmpty() ? captionToID(ID_PREFIX, form, caption) : id),
				caption);
		this.noColumn = true;
		this.optional = Optionalness.ALWAYS;
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
	public <V, UI extends CollectorUI<V, UI>> LabelUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createLabelUI(this);
	}
	
	@Override
	public void setNoColumn(boolean noColumn)
	{
		//form.addWarning("Ignored noColumn setting on Label (it never has a column)");
		//throw new UnsupportedOperationException("setNoColumn is unsupported on LabelFields since they never have columns.");
	}
	
	@Override
	public void setOptional(Optionalness optionalness)
	{
		// does nothing, labels are always optional
	}
	
}