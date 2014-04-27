package uk.ac.ucl.excites.sapelli.collector.control;

import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;

public class FieldWithArguments
{
	
	protected Field field;
	protected FieldParameters arguments;
	
	/**
	 * @param field the field
	 */
	public FieldWithArguments(Field field)
	{
		this(field, FieldParameters.EMPTY);
	}
	
	/**
	 * @param field the field
	 * @param arguments arguments passed along by previous field
	 */
	public FieldWithArguments(Field field, FieldParameters arguments)
	{
		this.field = field;
		this.arguments = new FieldParameters(arguments); // create a copy!
	}
	
}
