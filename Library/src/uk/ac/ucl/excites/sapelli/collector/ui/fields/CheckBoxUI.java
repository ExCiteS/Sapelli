package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;


/**
 * @author mstevens
 *
 */
public abstract class CheckBoxUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<CheckBoxField, V, UI>
{

	public CheckBoxUI(CheckBoxField checkBox, Controller controller, UI collectorUI)
	{
		super(checkBox, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(Record record)
	{
		field.getColumn().storeValue(record, getValue());
	}
	
	protected abstract boolean getValue();

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(Record record)
	{
		return true;
	}

}
