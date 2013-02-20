package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.ui.FieldView;
import uk.ac.ucl.excites.storage.model.IntegerColumn;

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
			valueDict = root.valueDict; //children share the valueDict of the root (so there is only 1 instance per choice tree)
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
	@Override
	public Choice getRoot()
	{
		return root;
	}
	
	@Override
	public boolean isRoot()
	{
		return parent == null;
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
	
	@Override
	public Field getJump()
	{
		if(jump == null && parent != null)
			return parent.getJump(); //return jump of parent
		else
			return jump; //return own jump (possibly null)
	}
	
	@Override
	public boolean isNoColumn()
	{
		return root.noColumn; //!!!
	}
	
	@Override
	protected IntegerColumn createColumn()
	{
		if(!isRoot())
			throw new IllegalStateException("createColumn() should only be called on a root Choice object.");
		//Build value dictionary:	
		addValues(); //Finds & adds the values for all leafs
		//Create & add column:
		return new IntegerColumn(id, true /* TODO determine if truly optional */, 0, valueDict.keySet().size() - 1);
	}

	/**
	 * Recursive method which implements a depth-first traversal that finds the values of all leafs.
	 * If a leaf does not have a value of it's own the one of a parent is found using getValue(), if that
	 * still does not result in a (non-null) value the id of the leaf is use (and kept) as its value.<br/>
	 * <br/>
	 * <b>Note 1:</b> This method should only be called after the whole choice tree is parsed & constructed (i.e. from addColumns()).<br/>
	 * <b>Note 2:</b> This traversal strategy is better than adding values to the valueDict at in the constructor or in setValue()
	 * because that could result in valueDict containing values that can never be chosen (namely when a non-leaf
	 * has a value but all the leafs below it have values of their own).
	 */
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
			for(Choice child : children) //Depth-first traversal
				child.addValues(); //recursive call
	}
	
	public void storeValue(FormEntry entry)
	{
		((IntegerColumn) entry.getColumn(root.id)).storeValue(entry, Long.valueOf(lookupValueCode()));
	}
	
	public int lookupValueCode()
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

	@Override
	public void setIn(FieldView fv)
	{
		fv.setChoice(this);
	}
	
	public String toString()
	{
		return "Choice " + id + (value != null ? " (value: " + value + ")" : " (no value set)");
	}
	
}
