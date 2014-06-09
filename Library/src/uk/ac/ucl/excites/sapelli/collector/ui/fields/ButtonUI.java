package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField.ButtonColumnType;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;

public abstract class ButtonUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<ButtonField, V, UI>
{

	/**
	 * @param field
	 * @param controller
	 */
	public ButtonUI(ButtonField field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	/**
	 * If the button has a boolean column but it is not pressed the 'null' value in
	 * the column should still be changed to 'false' to indicate that the button was
	 * shown to the user (i.e. the field was reached), and to ensure the record can
	 * be transmitted even if the button is non-optional.
	 * 
	 * Note 1:	This will only happen if the button was shown to the user. So *not*
	 * 			when the button (or the page that contains it) was not reached, or
	 * 			when it was hidden due to not being allowed to show on create/edit.
	 * 
	 * Note 2:	We do *not* have a similar mechanism for datetime columns, so it is
	 * 			recommended to always make buttons with a datetime column optional
	 * 			(a warning is shown if this is not the case).
	 * 
	 * Note 3:	The reason we to this upon validation instead of leaving, is that
	 * 			because if we would do it upon leaving, unpressed boolean-column
	 * 			non-optional buttons on pages would return false on isValid(),
	 * 			preventing leave() from being called at all.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.SelfLeavingFieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(Record record)
	{
		// Replace 'null' by 'false' in boolean column:
		if(field.getColumnType() == ButtonColumnType.BOOLEAN)
		{
			BooleanColumn column = (BooleanColumn) field.getColumn();
			if(!column.isValueSet(record)) // if current value is null (and only then!)...
				column.storeValue(record, false); // save 'false' to indicate that the button was *not* (yet) pressed
		}
		
		return super.isValid(record);
	}
	
	protected void buttonPressed()
	{
		Record record = controller.getCurrentRecord();
		
		// Store boolean or datetime:
		if(field.getColumnType() == ButtonColumnType.BOOLEAN)
			((BooleanColumn) field.getColumn()).storeValue(record, true);
		else if(field.getColumnType() == ButtonColumnType.DATETIME)
			((TimeStampColumn) field.getColumn()).storeValue(record, TimeStamp.now());
		
		// Continue to jump or next:
		if(field.getJump() != null)
			controller.goTo(new FieldWithArguments(field.getJump(), field.getNextFieldArguments()));
		else
			controller.goForward(true);
		
		/* Note:
		 * Simply calling goForward() is not a good idea because if the button is on a page (which is usually the case)
		 * the current field of the controller is the page, not the button, so in that case goForward() will go to the
		 * jump/next of the page, instead of the jump/next of the button. Therefore we first check if he button has a
		 * jump and if it has one we "goTo()" there. If it does not have a jump we call goForward() which will take us
		 * to the next/jump of the page (if the button is on one) or to the next of the button (if it is not on a page).
		 */
	}

}
