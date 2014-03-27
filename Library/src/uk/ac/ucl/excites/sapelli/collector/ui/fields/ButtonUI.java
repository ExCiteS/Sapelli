package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;

public abstract class ButtonUI<V> extends SelfLeavingFieldUI<ButtonField, V>
{

	/**
	 * @param field
	 * @param controller
	 */
	public ButtonUI(ButtonField field, Controller controller, CollectorUI<V> collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	protected void buttonPressed()
	{
		//TODO store boolean/datetime
		
		// Continue to jump:
		//TODO use controller.goForward() instead?
		controller.goTo(field.getJump());
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
