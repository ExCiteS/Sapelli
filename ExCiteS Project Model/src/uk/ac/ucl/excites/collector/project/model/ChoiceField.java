package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;


/**
 * @author mstevens
 *
 */
public class ChoiceField extends Field
{
	
	static public final int DEFAULT_NUM_COLS = 2;
	
	private ChoiceField parent;
	private ChoiceField root;
	private List<ChoiceField> children;
	private String imageLogicalPath;
	private int cols;
	private int rows;
	private String alt;
	private String value;
	private ValueDictionary valueDict;
	
	public ChoiceField(Form form, String id, ChoiceField parent)
	{
		super(	form,
				id == null ? 
					(parent == null ?
						null /*Field constructor will throw NPE*/ :
						parent.getID() + "." + parent.getChildren().size()) :
					id);
		this.children = new ArrayList<ChoiceField>();
		this.parent = parent;
		if(parent == null)
		{	//this is a root choice
			root = this; //self-pointer
			valueDict = new ValueDictionary(); //root holds the dictionary
		}
		else
		{	//this is a child choice
			parent.addChild(this); //add myself as a child of my parent
			root = parent.root;
			valueDict = root.valueDict; //children share the valueDict of the root (so there is only 1 instance per choice tree)
		}
	}
	
	public void addChild(ChoiceField c)
	{
		children.add(c);
	}

	/**
	 * @return the imageLogicalPath
	 */
	public String getImageLogicalPath()
	{
		return imageLogicalPath;
	}

	/**
	 * @param imageLogicalPath the imageLogicalPath to set
	 */
	public void setImageLogicalPath(String imageLogicalPath)
	{
		this.imageLogicalPath = imageLogicalPath;
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
	public ChoiceField getParent()
	{
		return parent;
	}
	
	/**
	 * Returns the root of this choice tree. This can be the same object (i.e. 'this') if it is the root.
	 * 
	 * @return the root
	 */
	@Override
	public ChoiceField getRoot()
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
	public List<ChoiceField> getChildren()
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
	public Optionalness getOptional()
	{
		return root.optional;
	}
	
	@Override
	protected IntegerColumn createColumn()
	{
		if(!isRoot())
			throw new IllegalStateException("createColumn() should only be called on a root ChoiceField object.");
		buildValueDict(); //Finds & adds the values for all leafs
		//Create & add column:
		return new IntegerColumn(id, (optional != Optionalness.NEVER), 0, valueDict.size() - 1);
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
	private void buildValueDict()
	{
		if(isLeaf())
		{
			if(getValue() == null || getValue().isEmpty()) //getValue() will return the value of the ChoiceField or of the/a parent, if there's no value set anywhere it will return null
				setValue(id); //id becomes value for this leaf ChoiceField; after this getValue() will never again return null for this ChoiceField object
			valueDict.addValue(getValue());
		}
		else
			for(ChoiceField child : children) //Depth-first traversal
				child.buildValueDict(); //recursive call
	}
	
	public void storeValue(Record entry)
	{
		if(!isNoColumn())
			((IntegerColumn) root.column).storeValue(entry, Long.valueOf(lookupCode()));
	}
	
	public int lookupCode()
	{
		return valueDict.lookupCode(getValue());
	}

	@Override
	public void setIn(CollectorUI ui)
	{
		ui.setChoice(this);
	}
	
	public String toString()
	{
		return "ChoiceField " + id + (value != null ? " (value: " + value + ")" : " (no value set)");
	}
	
}
