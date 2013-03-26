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
	
	static public final int NON_LEAF_VALUE_INDEX = -1;
	static public final int DEFAULT_NUM_COLS = 2;
	
	private ChoiceField parent;
	private ChoiceField root;
	private List<ChoiceField> children;
	private String imageLogicalPath;
	private int cols;
	private int rows;
	private String alt;
	private String value;
	private int finalValueIndex;
	private List<ChoiceField> finalValueChoices;
	
	public ChoiceField(Form form, String id, String value, ChoiceField parent)
	{
		super(	form,
				id == null || id.isEmpty() ?
					(parent == null ?
						null /* id is mandatory for the root: Field constructor will throw NullPointerException */ :
						/* generate id based on parent ID and value or child number: */
						parent.getID() + "." + (value == null || value.isEmpty() ?
													parent.getChildren().size() + 1 :
													value)) :
					id);
		this.children = new ArrayList<ChoiceField>();
		this.parent = parent;
		this.value = ((value == null || value.isEmpty()) ? null : value); //replace empty string with null (so we don't need to check for empty string elsewhere)
		this.finalValueIndex = NON_LEAF_VALUE_INDEX; //will be changed for leaf choices in findFinalValues()
		if(parent == null)
		{	//this is a root choice
			root = this; //self-pointer
			finalValueChoices = new ArrayList<ChoiceField>(); //root holds the finalValueChoices list
		}
		else
		{	//this is a child choice
			parent.addChild(this); //add myself as a child of my parent
			root = parent.root;
			finalValueChoices = root.finalValueChoices; //children share the finalValueChoices list of the root (so there is only 1 instance per choice tree)
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
		this.findFinalValues(); //!!!
		if(finalValueChoices.isEmpty())
		{	//no values set
			form.addWarning("noColumn was forced to true on ChoiceField " + getID() + " because it has no values.");
			noColumn = true; //!!!
			return null;
		}
		else
		{	//Create column:
			return new IntegerColumn(id, (optional != Optionalness.NEVER), 0, finalValueChoices.size() - 1);
		}
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
	
	private ChoiceField getLowestAncestorWithValue()
	{
		if(value == null && parent != null)
			return parent.getLowestAncestorWithValue();
		else
			return this; //return self
	}
	
	/**
	 * Recursive method which implements a depth-first traversal that finds all leaves.
	 * <br/>
	 * <b>Note:</b> This method should only be called after the whole choice tree is parsed & constructed (i.e. from addColumns()).<br/>
	 */
	private void findFinalValues()
	{
		if(isLeaf())
		{
			ChoiceField valuedChoice = this.getLowestAncestorWithValue();
			if(valuedChoice.getValue() != null) //there must be at least one non-null value
			{
				//add to list of final choices and store index in the leaf (not in the valuedChoice!):
				int index = finalValueChoices.indexOf(valuedChoice);
				if(index == -1)
				{
					this.finalValueIndex = finalValueChoices.size();
					finalValueChoices.add(valuedChoice);
				}
				else
					//valuedChoice was already in the list:
					this.finalValueIndex = index;
			}
		}
		else
		{
			for(ChoiceField child : children) //Depth-first traversal
				child.findFinalValues(); //recursive call
		}
	}
	
	public void storeValue(Record entry)
	{
		if(!isNoColumn() && isLeaf())
			((IntegerColumn) root.column).storeValue(entry, Long.valueOf(finalValueIndex));
	}
	
	public ChoiceField lookupChoice(int valueIndex)
	{
		return finalValueChoices.get(valueIndex);
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
