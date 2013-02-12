package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

/**
 * @author mstevens
 *
 */
public class Choice extends Field
{

	static public final int UNKNOWN_VALUE_CODE = -1;
	
	private Choice parent;
	private Choice root;
	private List<Choice> children;
	private String imagePath;
	private int cols;
	private int rows;
	private String alt;
	private String value;
	private BiMap<String, Integer> valueDict;
	
	public Choice(String id, Choice parent)
	{
		super(id);
		this.children = new ArrayList<Choice>();
		this.parent = parent;
		if(parent == null)
		{	//this is a root choice
			if(id == null)
				throw new NullPointerException("ID cannot be null on a root (i.e. top-level) Choice.");
			root = this; //self-pointer
			valueDict = HashBiMap.create(); //value dictionary
		}
		else
		{	//this is a child choice
			parent.addChild(this); //add myself as a child of my parent
			root = parent.root;
			if(id == null)
				this.id = parent.getID() + "." + parent.getChildren().size();
			valueDict = root.valueDict; //children share the valueDict of the root (so there is only one per choice tree)
		}
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
		if(value == null && parent != null)
			return parent.getValue(); //return value of parent
		else
			return value; //return own value (possibly null)
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
	 * Returns the root of this choice tree. This can be the same object (i.e. 'this') if it is the root.
	 * 
	 * @return the root
	 */
	public Choice getRoot()
	{
		return root;
	}

	/**
	 * @return the children
	 */
	public List<Choice> getChildren()
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
	
	public boolean isLeaf()
	{
		return children.isEmpty();
	}
	
	public boolean isRoot()
	{
		return parent == null;
	}
	
	@Override
	public Field getJump()
	{
		if(jump == null && parent != null)
			return parent.getJump(); //return jump of parent
		else
			return jump; //return own jump (possibly null)
	}
	
	@Override
	public void addColumns(Schema schema)
	{
		if(!isRoot() || noColumn)
			return;
		//Build value dictionary:
		addValues(); //Depth-first-traveral to find all leafs/values
		//Create & add column:
		schema.addColumn(new IntegerColumn(id, true /* TODO determine if truly optional */, 0, valueDict.keySet().size() - 1));
	}
	
	private void addValues()
	{
		if(isLeaf())
		{
			if(getValue() == null) //getValue() will return the value of the Choice or of the/a parent, if there's no value set anywhere it will return null
				setValue(id); //id becomes value for this leaf Choice; after this getValue() will never again return null for this Choice object
			if(!valueDict.containsKey(getValue()))
				valueDict.put(getValue(), Integer.valueOf(valueDict.keySet().size()));
		}
		else
			for(Choice child : children) //Depth-first-traversal
				child.addValues(); //recursive call
	}
	
	public void storeValue(Record record)
	{
		((IntegerColumn) record.getSchema().getColumn(root.id)).storeValue(record, Long.valueOf(lookupValueCode()));
	}
	
	private int lookupValueCode()
	{
		return lookupValueCode(getValue());
	}
	
	public int lookupValueCode(String value)
	{
		Integer code = valueDict.get(value);
		return (code != null ? code : UNKNOWN_VALUE_CODE);
	}
	
	public String lookupValue(int valueCode)
	{
		return valueDict.inverse().get(Integer.valueOf(valueCode));
	}
	
}
