/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;


/**
 * @author mstevens
 *
 * @param <V>
 * @param <UI>
 */
public abstract class TextBoxUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<TextBoxField, V, UI>
{

	// STATIC -------------------------------------------------------
	static protected final String NULL_MODE_CREATE = "— Touch to set —"; //TODO multilang
	static protected final String NULL_MODE_EDIT = "— Touch to edit —"; //TODO multilang
	static protected final String NULL_MODE_DISABED = "(no value set)"; //TODO multilang
	
	// DYNAMIC ------------------------------------------------------
	public TextBoxUI(TextBoxField textBox, Controller controller, UI collectorUI)
	{
		super(textBox, controller, collectorUI);
	}
	
	@Override
	public boolean isValid(Record record)
	{
		String text = getValue();
		String error = null;
		// Null check:
		if(text == null)
		{
			if(field.getOptional() != Optionalness.ALWAYS)
				 // this should never happen really
				error = "Non-optional field requires a value."; //TODO multilang
		}
		// Too short:
		else if(text.length() < field.getMinLength())
			error = "Minimum length of " + field.getMinLength() + " characters not reached."; //TODO multilang
		// Too long:
		else if(text.length() > field.getMaxLength())
			error = "Maximum length of " + field.getMaxLength() + " characters exceeded."; //TODO multilang
		// Match regular expression:
		else if(field.getRegexPattern() != null && !field.getRegexPattern().matcher(text).matches())
			error = "Pattern mismatch"; //TODO multilang
		// Column-level validation:
		else
			switch(field.getContent())
			{
				case text :
				case password :
				case email :
				case phonenumber :
				default :
					if(!field.getColumn().isValidValue(text))
						// in fact this shouldn't happen, given that max length has already been checked
						error = "Invalid input."; //TODO multilang
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
						error = "Invalid numeric input."; //TODO multilang
					break;
			}
		// Deal with error:
		if(error == null)
		{
			clearValidationError();
			return true;
		}
		else
		{
			if(field.getOptional() == Optionalness.ALWAYS)
				error += "\nIf you don't want to answer, hit backspace until cross appears."; //TODO multilang
			setValidationError(error);
			return false;
		}
	}
	
	/**
	 * 
	 * @throws Exception 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.NonSelfLeavingFieldUI#storeValue(Record)
	 */
	@Override
	protected void storeValue(Record record) throws Exception
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
	
	protected String retrieveValue(Record record)
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
	
	protected abstract String getValue();
	
	protected abstract void setValidationError(String errorDescr);
	
	protected abstract void clearValidationError();

}
