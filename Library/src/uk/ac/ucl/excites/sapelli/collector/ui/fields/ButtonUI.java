package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField.ButtonColumnType;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;

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
		CollectorRecord record = controller.getCurrentRecord();
		
		// Store boolean/datetime:
		if(field.getColumnType() == ButtonColumnType.BOOLEAN)
			((BooleanColumn) field.getColumn()).storeValue(record, true);
		else if(field.getColumnType() == ButtonColumnType.DATETIME)
			((DateTimeColumn) field.getColumn()).storeValue(record, DateTime.now());
		
		// Continue to jump or next:
		if(field.getJump() != null)
			controller.goTo(field.getJump());
		else
			controller.goForward(true);
		
		/* Note:
		 * Simply calling goForward() is not a good idea because if the button is on a page (which is usually the case)
		 * the current field of the controller is the page, not the button, so in that case goForward() will go to the
		 * jump/next of the page, instead of the jump/next of the button. Therefore we first check if he button has a
		 * jump and if it has so goTo there. If it does not have a jump we go the the next of the button or, more likely
		 * the page that contains it, by means of goForward().
		 */
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
