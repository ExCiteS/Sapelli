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
	static protected final int VALIDATION_ERROR_NON_OPTIONAL_MISSING = 1;
	static protected final int VALIDATION_ERROR_TOO_SHORT = 2;
	static protected final int VALIDATION_ERROR_TOO_LONG = 3;
	static protected final int VALIDATION_ERROR_PATTERN_MISMATCH = 4;
	static protected final int VALIDATION_ERROR_INVALID = 5;
	static protected final int VALIDATION_ERROR_INVALID_NUMERIC = 6;
	
	// DYNAMIC ------------------------------------------------------
	public TextBoxUI(TextBoxField textBox, Controller controller, UI collectorUI)
	{
		super(textBox, controller, collectorUI);
	}

	@Override
	public boolean isValid(Record record)
	{
		String text = getValue();
		Integer error = null;
		Integer errorArg = null;
		// Null check:
		if(text == null)
		{
			if(!field.isOptional())
				 // this should never happen really
				error = VALIDATION_ERROR_NON_OPTIONAL_MISSING;
		}
		// Too short:
		else if(text.length() < field.getMinLength())
		{
			error = VALIDATION_ERROR_TOO_SHORT;
			errorArg = field.getMinLength();
		}
		// Too long:
		else if(text.length() > field.getMaxLength())
		{
			error = VALIDATION_ERROR_TOO_LONG;
			errorArg = field.getMaxLength();
		}
		// Match regular expression:
		else if(field.getRegexPattern() != null && !field.getRegexPattern().matcher(text).matches())
			error = VALIDATION_ERROR_PATTERN_MISMATCH;
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
						error = VALIDATION_ERROR_INVALID;
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
						error = VALIDATION_ERROR_INVALID_NUMERIC;
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
			setValidationError(error, errorArg, field.isOptional());
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

	protected abstract void setValidationError(int errorCode, Integer argument, boolean hintOptional);

	protected abstract void clearValidationError();
	
}
