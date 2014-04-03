/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;

/**
 * @author mstevens
 *
 */
public abstract class MultiListUI<V> extends NonSelfLeavingFieldUI<MultiListField, V>
{

	static protected final String PLEASE_SELECT = "— Please select —"; //TODO multilang
	static protected final String UNDO_SELECTION = "— No selection —"; //TODO multilang
	
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
		MultiListItem chosen = getChosenItem();
		if(!field.isNoColumn() && chosen != null)
			field.getColumn().storeValue(record, field.getValueForItem(chosen));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		MultiListItem chosen = getChosenItem();
		boolean valid = chosen == null ? (field.getOptional() == Optionalness.ALWAYS) : chosen.isLeaf();
		return valid;
	}
	
	protected abstract MultiListItem getChosenItem();

}
