package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;


/**
 * @author mstevens
 *
 */
public abstract class CheckBoxUI<V> extends NonSelfLeavingFieldUI<CheckBoxField, V>
{

	public CheckBoxUI(CheckBoxField checkBox, Controller controller, CollectorUI<V> collectorUI)
	{
		super(checkBox, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(CollectorRecord record)
	{
		((BooleanColumn) field.getColumn()).storeValue(record, getValue());
	}
	
	protected abstract boolean getValue();

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		return true;
	}

}
