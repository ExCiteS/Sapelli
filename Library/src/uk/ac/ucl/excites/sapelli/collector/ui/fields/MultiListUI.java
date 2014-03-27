/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;

/**
 * @author mstevens
 *
 */
public abstract class MultiListUI<V> extends NonSelfLeavingFieldUI<MultiListField, V>
{

	static protected final String PLEASE_SELECT = "— Please select —"; //TODO multilang
	
	public MultiListUI(MultiListField listField, Controller controller, CollectorUI<V> collectorUI)
	{
		super(listField, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(CollectorRecord record)
	{
		if(!field.isNoColumn())
			((IntegerColumn) field.getColumn()).storeValue(record, field.getDictionary().lookupIndex(getChosenItem()));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		// TODO multilist validation
		return false; // are we at leaf?
	}
	
	protected abstract MultiListItem getChosenItem();

}
