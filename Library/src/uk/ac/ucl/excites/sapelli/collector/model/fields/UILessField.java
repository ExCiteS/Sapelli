/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;

/**
 * @author mstevens
 */
public abstract class UILessField extends Field
{

	/**
	 * @param form
	 * @param id
	 */
	public UILessField(Form form, String id)
	{
		super(form, id, null);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> FieldUI<?, V, UI> createUI(UI collectorUI)
	{
		return null;
	}

}
