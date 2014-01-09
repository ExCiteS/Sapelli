/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.fields.Field;

/**
 * A Page of a {@link Form}.
 * 
 * @author mstevens
 *
 */
public class FormPage
{
	
	private List<Field> fields;

	/**
	 * 
	 */
	public FormPage()
	{
		fields = new ArrayList<Field>();
	}
	
	/**
	 * 
	 */
	public FormPage(Field singleField)
	{
		fields = new ArrayList<Field>(1);
		fields.add(singleField);
	}

	public void addField(Field field)
	{
		fields.add(field);
	}

	public List<Field> getFields()
	{
		return fields;
	}
	
}
