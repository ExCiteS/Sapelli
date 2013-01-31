package uk.ac.ucl.excites.collector.model;

import java.util.ArrayList;

/**
 * @author mstevens
 *
 */
public class Choice extends Field
{

	private Choice parent;
	private ArrayList<Choice> children;
	private String imagePath;
	private int cols;
	private int rows;
	private String alt;
	private String value;

	public Choice()
	{
		this(null);
	}
	
	public Choice(Choice parent)
	{
		this.parent = parent;
	}
	
	public void addChild(Choice c)
	{
		children.add(c);
	}
	
	/**
	 * @return the imagePath
	 */
	public String getImagePath()
	{
		return imagePath;
	}

	/**
	 * @param imagePath the imagePath to set
	 */
	public void setImagePath(String imagePath)
	{
		this.imagePath = imagePath;
	}

	/**
	 * @return the alt
	 */
	public String getAlt()
	{
		return alt;
	}

	/**
	 * @param alt the alt to set
	 */
	public void setAlt(String alt)
	{
		this.alt = alt;
	}

	/**
	 * @return the value
	 */
	public String getValue()
	{
		return value;
	}

	/**
	 * @param value the value to set
	 */
	public void setValue(String value)
	{
		this.value = value;
	}

	/**
	 * @return the parent
	 */
	public Choice getParent()
	{
		return parent;
	}

	/**
	 * @return the children
	 */
	public ArrayList<Choice> getChildren()
	{
		return children;
	}

	/**
	 * @return the cols
	 */
	public int getCols()
	{
		return cols;
	}

	/**
	 * @param cols the cols to set
	 */
	public void setCols(int cols)
	{
		this.cols = cols;
	}

	/**
	 * @return the rows
	 */
	public int getRows()
	{
		return rows;
	}

	/**
	 * @param rows the rows to set
	 */
	public void setRows(int rows)
	{
		this.rows = rows;
	}
	
}
