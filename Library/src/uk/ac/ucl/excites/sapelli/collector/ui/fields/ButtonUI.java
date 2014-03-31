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
	
	/**
	 * If the button has a boolean columns but it is not pressed the 'null' value
	 * in the column should still be changed to 'false' to indicate that the button
	 * was shown to the user (i.e. the field was reached), and to ensure the record
	 * can be saved if the button is non-optional.
	 * 
	 * We do not have a similar mechanism for datatime columns, so it is recommended
	 * to always make buttons with a datatime column optional (which they are by default).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI#leave(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord, boolean)
	 */
	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		if(field.getColumnType() == ButtonColumnType.BOOLEAN)
		{
			BooleanColumn column = (BooleanColumn) field.getColumn();
			if(!column.isValueSet(record)) // if value is null (and only then!)...
				column.storeValue(record, false); // save 'false' to indicate that the button was not pressed
		}
		return true;
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
