package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;


/**
 * @author mstevens
 *
 */
public abstract class LabelUI<V> extends NonSelfLeavingFieldUI<LabelField, V>
{

	public LabelUI(LabelField labelField, Controller controller, CollectorUI<V> collectorUI)
	{
		super(labelField, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(CollectorRecord record)
	{
		// does nothing (labels never have a column)
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		return true;
	}

}
