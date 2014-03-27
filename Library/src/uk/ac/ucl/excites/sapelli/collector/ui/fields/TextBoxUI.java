package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;


/**
 * @author mstevens
 */
public abstract class TextBoxUI<V> extends NonSelfLeavingFieldUI<TextBoxField, V>
{

	public TextBoxUI(TextBoxField textBox, Controller controller, CollectorUI<V> collectorUI)
	{
		super(textBox, controller, collectorUI);
	}
	
	/**
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public void storeValue(CollectorRecord record)
	{
		((StringColumn) field.getColumn()).storeValue(record, getValue());	
	}
	
	@Override
	public boolean isValid(CollectorRecord record)
	{
		// TODO deal with optionallity? minimal lenths?
		String text = getValue();
		// Too short:
		if(text.length() < field.getMinLength())
		{
			setValidationError("Minimum length of " + field.getMinLength() + " characters not reached."); //TODO multilang
			return false;
		}
		// Too long:
		else if(text.length() > field.getMaxLength())
		{
			setValidationError("Maximum length of " + field.getMaxLength() + " characters exceeded."); //TODO multilang
			return false;
		}
		// OK:
		clearValidationError();
		return true;
	}
	
	protected abstract String getValue();
	
	protected abstract void setValidationError(String errorDescr);
	
	protected abstract void clearValidationError();

}
