/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * FieldUI for MultiList fields
 * 
 * TODO maybe allow list navigation/selection by keystrokes (in which case the keyboard should be hidden in cancel())
 * 
 * @author mstevens
 */
public abstract class MultiListUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<MultiListField, V, UI>
{

	// STATIC -------------------------------------------------------
	static protected final String PLEASE_SELECT = "— Please select —"; //TODO multilang
	static protected final String UNDO_SELECTION = "— No selection —"; //TODO multilang
	
	// DYNAMIC ------------------------------------------------------
	public MultiListUI(MultiListField listField, Controller controller, UI collectorUI)
	{
		super(listField, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(Record record)
	{
		MultiListItem chosen = getChosenItem();
		if(!field.isNoColumn())
			field.getColumn().storeValue(record, chosen != null ? field.getValueForItem(chosen) : null);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(Record record)
	{
		MultiListItem chosen = getChosenItem();
		return chosen == null ? (field.getOptional() == Optionalness.ALWAYS) : chosen.isLeaf();
	}
	
	protected abstract MultiListItem getChosenItem();

}
