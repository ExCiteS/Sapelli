/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import java.util.ArrayList;

/**
 * @author mstevens
 *
 */
public class Form
{
	
	private Field start;
	private int endAction;
	//Schema
	private ArrayList<Field> fields;
	private boolean shortcut;
	//shortcutIcon 
	private boolean vibrateOnEnd;
	private String endSoundPath;
	
	private boolean showBack;
	private boolean showHome;
	
	public Form()
	{
		this.fields = new ArrayList<Field>();
	}
	
	public void addField(Field f)
	{
		fields.add(f);
		if(!f.noColumn)
		{
			//TODO add a column to the schema
		}
	}

	/**
	 * @return the start
	 */
	public Field getStart()
	{
		return start;
	}

	/**
	 * @param start the start to set
	 */
	public void setStart(Field start)
	{
		this.start = start;
	}
	
}
