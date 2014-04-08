/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.TextBoxUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import android.annotation.SuppressLint;

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
	public static final int DEFAULT_MIN_LENGTH_OPTIONAL = 0;		// default minimum length of 0 chars if field is optional (i.e. optionality = ALWAYS)
	public static final int DEFAULT_MIN_LENGTH_NON_OPTIONAL = 1;	// default minimum length of 1 char if field is not optional (i.e. optionality = NEVER or NOT_WHEN_REACHED)
	public static final int DEFAULT_MAX_LENGTH = 128; 				// default maximum length of 128 chars
	public static final boolean DEFAULT_MULTILINE = false;			// single-line by default
	public static final String DEFAULT_INITIAL_VALUE = "";			// empty String is the default/initial initialValue
	public static final Content DEFAULT_CONTENT = Content.text;		// plain Text content by default
	public static final Capitalisation DEFAULT_CAPITALISATION = Capitalisation.none; // use no automatic capitalisation by default
	
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
		this.maxLength = DEFAULT_MAX_LENGTH;
		this.minLength = DEFAULT_MIN_LENGTH_OPTIONAL;
		this.multiline = DEFAULT_MULTILINE;
		this.initialValue = DEFAULT_INITIAL_VALUE;
		this.content = DEFAULT_CONTENT;
		this.capitalisation = DEFAULT_CAPITALISATION;
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

	/**
	 * @return the multiline
	 */
	public boolean isMultiline()
	{
		return multiline;
	}

	/**
	 * @return the initValue
	 */
	public String getInitialValue()
	{
		return initialValue;
	}

	/* (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		switch(content)
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				return StringColumn.ForCharacterCount(id, optional != Optionalness.NEVER, maxLength);
			case unsignedint :
				return new IntegerColumn(id, optional != Optionalness.NEVER, false, Integer.SIZE);
			case signedint :
				return new IntegerColumn(id, optional != Optionalness.NEVER, true, Integer.SIZE);
			case unsignedlong :
				return new IntegerColumn(id, optional != Optionalness.NEVER, false, Long.SIZE);
			case signedlong :
				return new IntegerColumn(id, optional != Optionalness.NEVER, true, Long.SIZE);
			case unsignedfloat :
				return new FloatColumn(id, optional != Optionalness.NEVER, false, false);
			case signedfloat :
				return new FloatColumn(id, optional != Optionalness.NEVER, true, false);
			case unsigneddouble :
				return new FloatColumn(id, optional != Optionalness.NEVER, false, true);
			case signeddouble :
				return new FloatColumn(id, optional != Optionalness.NEVER, true, true);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, boolean withPage)
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

	public void setMaxLength(int maxLength)
	{
		this.maxLength = maxLength;
	}

	public int getDefaultMinLength()
	{
		return optional == Optionalness.ALWAYS ? DEFAULT_MIN_LENGTH_OPTIONAL : DEFAULT_MIN_LENGTH_NON_OPTIONAL;
	}
	
	public void setMinLength(int minLength)
	{
		this.minLength = minLength;
	}

	public void setMultiline(boolean multiline)
	{
		this.multiline = multiline;
	}

	public void setInitialValue(String initValue)
	{
		this.initialValue = initValue;
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
	@SuppressLint("DefaultLocale")
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
	@SuppressLint("DefaultLocale")
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
	
}
