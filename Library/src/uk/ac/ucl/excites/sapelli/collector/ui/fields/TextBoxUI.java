package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;


/**
 * @author mstevens
 */
public abstract class TextBoxUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<TextBoxField, V, UI>
{

	public TextBoxUI(TextBoxField textBox, Controller controller, UI collectorUI)
	{
		super(textBox, controller, collectorUI);
	}
	
	/**
	 * 
	 * @throws Exception 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(Record)
	 */
	@Override
	public void storeValue(Record record) throws Exception
	{
		switch(field.getContent())
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				((StringColumn) field.getColumn()).storeValue(record, getValue()); break;
			case unsignedint :
			case signedint :
			case unsignedlong :
			case signedlong :
			case unsignedfloat :
			case signedfloat :
			case unsigneddouble :
			case signeddouble :
				field.getColumn().parseAndStoreValue(record, getValue()); break; // parse number & store it
		}
	}
	
	public String retrieveValue(Record record)
	{
		if(!field.getColumn().isValueSet(record))
			return null;
		switch(field.getContent())
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				return ((StringColumn) field.getColumn()).retrieveValue(record);
			case unsignedint :
			case signedint :
			case unsignedlong :
			case signedlong :
			case unsignedfloat :
			case signedfloat :
			case unsigneddouble :
			case signeddouble :
				return field.getColumn().retrieveValueAsString(record); // retrieve as String
		}
	}
	
	@Override
	public boolean isValid(Record record)
	{
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
		// Match regular expression:
		if(field.getRegexPattern() != null && !field.getRegexPattern().matcher(text).matches())
		{
			setValidationError("Pattern mismatch"); //TODO multilang
			return false;
		}
		// Column-level validation:
		switch(field.getContent())
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				if(!field.getColumn().isValidValue(text))
				{	// this shouldn't happen, given that max length has already been checked
					setValidationError("Invalid input."); //TODO multilang
					return false;
				}
				break;
			case unsignedint :
			case signedint :
			case unsignedlong :
			case signedlong :
			case unsignedfloat :
			case signedfloat :
			case unsigneddouble :
			case signeddouble :
				if(!field.getColumn().isValidValueString(text))
				{
					setValidationError("Invalid numeric input."); //TODO multilang
					return false;
				}
				break;
		}
		// OK:
		clearValidationError();
		return true;
	}
	
	protected abstract String getValue();
	
	protected abstract void setValidationError(String errorDescr);
	
	protected abstract void clearValidationError();
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#usesKeyboard()
	 */
	@Override
	public boolean usesKeyboard()
	{
		return true;
	}

}
