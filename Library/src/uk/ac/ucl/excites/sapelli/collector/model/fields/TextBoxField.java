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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.TextBoxUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * A Field class representing a text box
 * 
 * @author mstevens, Julia
 */
public class TextBoxField extends Field
{

	static public final String ID_PREFIX = "txt";

	/**
	 * Content types
	 */
	static public enum Content
	{
		// Text-like (Android InputType class Text; stored as String):
		text,
		password,
		email,
		// Phone number (Android InputType class Phone; stored as String):
		phonenumber,
		// Numeric (Android InputType class Number; stored as Integer/Long or Float/Double):
		unsignedint,
		signedint,
		unsignedlong,
		signedlong,
		unsignedfloat,
		signedfloat,
		unsigneddouble,
		signeddouble,
	}
	
	/**
	 * Automatic capitalisation (only applies for Content=text)
	 */
	static public enum Capitalisation
	{
		none,
		all,
		words,
		sentences,
	}
	
	// Defaults
	static public final int DEFAULT_MIN_LENGTH_OPTIONAL = 0;			// default minimum length of 0 chars if field is optional (i.e. optionality = ALWAYS)
	static public final int DEFAULT_MIN_LENGTH_NON_OPTIONAL = 1;		// default minimum length of 1 char if field is not optional (i.e. optionality = NEVER or NOT_WHEN_REACHED)
	static public final int DEFAULT_MAX_LENGTH = 128; 					// default maximum length of 128 chars
	static public final boolean DEFAULT_MULTILINE = false;				// single-line by default
	static public final String DEFAULT_INITIAL_VALUE_OPTIONAL = null;	// null is the default initialValue if field is optional (i.e. optionality = ALWAYS)
	static public final String DEFAULT_INITIAL_VALUE_NON_OPTIONAL = "";	// empty String is the default initialValue if field is not optional (i.e. optionality = NEVER or NOT_WHEN_REACHED)
	static public final Content DEFAULT_CONTENT = Content.text;		// plain Text content by default
	static public final Capitalisation DEFAULT_CAPITALISATION = Capitalisation.none; // use no automatic capitalisation by default
	
	// Dynamics
	private int maxLength;
	private int minLength;
	private boolean multiline;
	private String initialValue;
	private Content content;
	private Capitalisation capitalisation;
	private Pattern regexPattern = null;
	
	/**
	 * @param form
	 * @param id
	 * @param caption
	 */
	public TextBoxField(Form form, String id, String caption)
	{
		super(	form,
				(id == null || id.isEmpty() ? captionToID(ID_PREFIX, form, caption) : id),
				caption);
		this.setMinMaxLength(getDefaultMinLength(), DEFAULT_MAX_LENGTH);
		this.setInitialValue(getDefaultInitialValue());
		this.multiline = DEFAULT_MULTILINE;
		this.content = DEFAULT_CONTENT;
		this.capitalisation = DEFAULT_CAPITALISATION;
	}

	public int getDefaultMinLength()
	{
		return optional == Optionalness.ALWAYS ? DEFAULT_MIN_LENGTH_OPTIONAL : DEFAULT_MIN_LENGTH_NON_OPTIONAL;
	}
	
	public void setMinMaxLength(int minLength, int maxLength)
	{
		if(minLength < 0 || maxLength < 1 || minLength > maxLength)
			throw new IllegalArgumentException("minLength must be greater or equal to 0, maxLength must be greater or equal to 1, and minLength must be smaller or equal to maxLength");
		this.minLength = minLength;
		this.maxLength = maxLength;
	}
	
	/**
	 * @return the maxLength
	 */
	public int getMaxLength()
	{
		return maxLength;
	}

	/**
	 * @return the minLength
	 */
	public int getMinLength()
	{
		return minLength;
	}
	
	public String getDefaultInitialValue()
	{
		return optional == Optionalness.ALWAYS ? DEFAULT_INITIAL_VALUE_OPTIONAL : DEFAULT_INITIAL_VALUE_NON_OPTIONAL;
	}
	
	/**
	 * @return the initValue
	 */
	public String getInitialValue()
	{
		return initialValue;
	}

	/**
	 * @param initValue
	 */
	public void setInitialValue(String initValue)
	{
		if(initValue == null && optional != Optionalness.ALWAYS)
			throw new IllegalArgumentException("Initial/default value can only be null if the field is (always) optional");
		if(initValue != null && initValue.length() > maxLength)
			throw new IllegalArgumentException("Initial/default value is too long (length: " + initValue.length() + "; max: " + maxLength + ")");
		this.initialValue = initValue;
	}

	/**
	 * @return the multiline
	 */
	public boolean isMultiline()
	{
		return multiline;
	}

	/**
	 * @param multiline
	 */
	public void setMultiline(boolean multiline)
	{
		this.multiline = multiline;
	}
	
	/**
	 * @return the content
	 */
	public Content getContent()
	{
		return content;
	}

	/**
	 * @param content the content to set
	 */
	public void setContent(Content content)
	{
		this.content = content;
	}
	
	/**
	 * @param contentStr the content (as String) to set
	 */
	public void setContent(String contentStr)
	{
		if(contentStr == null)
			return;
		try
		{
			this.content = Content.valueOf(contentStr.toLowerCase());
		}
		catch(IllegalArgumentException iae)
		{
			form.addWarning("Unrecognised capitalisation: " + contentStr);
		}
	}

	/**
	 * @return the regexPattern
	 */
	public Pattern getRegexPattern()
	{
		return regexPattern;
	}

	/**
	 * @param regex the regex to set
	 */
	public void setRegexPattern(String regex)
	{
		if(regex == null)
			this.regexPattern = null;
		else
			try
			{
				this.regexPattern = Pattern.compile(regex);
			}
			catch(PatternSyntaxException pse)
			{
				form.addWarning("Invalid regular expression (" + regex +") on field \'" + id + "\', no pattern check will be applied.");
			}
	}

	/**
	 * @return the capitalisation
	 */
	public Capitalisation getCapitalisation()
	{
		return capitalisation;
	}

	/**
	 * @param capitalisation the capitalisation to set
	 */
	public void setCapitalisation(Capitalisation capitalisation)
	{
		this.capitalisation = capitalisation;
	}
	
	/**
	 * @param capitalistationStr the capitalisation (as String) to set
	 */
	public void setCapitalisation(String capitalisationStr)
	{
		if(capitalisationStr == null)
			return;
		try
		{
			this.capitalisation = Capitalisation.valueOf(capitalisationStr.toLowerCase());
		}
		catch(IllegalArgumentException iae)
		{
			form.addWarning("Unrecognised capitalisation: " + capitalisationStr);
		}
	}
	
	/* (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected Column<?> createColumn(String name)
	{
		switch(content)
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				return StringColumn.ForCharacterCount(name, optional != Optionalness.NEVER, maxLength);
			case unsignedint :
				return new IntegerColumn(name, optional != Optionalness.NEVER, false, Integer.SIZE);
			case signedint :
				return new IntegerColumn(name, optional != Optionalness.NEVER, true, Integer.SIZE);
			case unsignedlong :
				return new IntegerColumn(name, optional != Optionalness.NEVER, false, Long.SIZE);
			case signedlong :
				return new IntegerColumn(name, optional != Optionalness.NEVER, true, Long.SIZE);
			case unsignedfloat :
				return new FloatColumn(name, optional != Optionalness.NEVER, false, false);
			case signedfloat :
				return new FloatColumn(name, optional != Optionalness.NEVER, true, false);
			case unsigneddouble :
				return new FloatColumn(name, optional != Optionalness.NEVER, false, true);
			case signeddouble :
				return new FloatColumn(name, optional != Optionalness.NEVER, true, true);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> TextBoxUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createTextFieldUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof TextBoxField)
		{
			TextBoxField that = (TextBoxField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.maxLength == that.maxLength &&
					this.minLength == that.minLength &&
					this.multiline == that.multiline &&
					(this.initialValue != null ? this.initialValue.equals(that.initialValue) : that.initialValue == null) &&
					this.content == that.content &&
					this.capitalisation == that.capitalisation &&
					(this.regexPattern != null ? that.regexPattern != null && this.regexPattern.toString().equals(that.regexPattern.toString()) : that.regexPattern == null);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + maxLength;
		hash = 31 * hash + minLength;
		hash = 31 * hash + (multiline ? 0 : 1);
		hash = 31 * hash + (initialValue == null ? 0 : initialValue.hashCode());
		hash = 31 * hash + content.ordinal();
		hash = 31 * hash + capitalisation.ordinal();
		hash = 31 * hash + (regexPattern == null ? 0 : regexPattern.toString().hashCode());
		return hash;
	}
	
}
