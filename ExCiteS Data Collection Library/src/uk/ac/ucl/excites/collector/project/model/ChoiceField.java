package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;


/**
 * Each ChoiceField represents a node in a decision tree. The whole of such a tree (starting with the root) describes the possible values the field (stored as an IntegerColumn) can take.
 * 
 * @author mstevens
 */
public class ChoiceField extends Field
{
	
	static public final int NON_LEAF_VALUE_INDEX = -1;
	static public final int NON_SELECTABLE_CHOICE_INDEX = -1;
	static public final int DEFAULT_NUM_COLS = 2;
	
	private ChoiceField parent;
	private ChoiceField root;
	private List<ChoiceField> children;
	private String imageLogicalPath;
	private int cols;
	private int rows;
	private String alt;
	private String value;
	private ChoiceDictionary dictionary;
	
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
		if(parent == null)
		{	//this is a root choice
			root = this; //self-pointer
			dictionary = new ChoiceDictionary(); //root holds the dictionary
		}
		else
		{	//this is a child choice
			parent.addChild(this); //add myself as a child of my parent
			root = parent.root;
			dictionary = root.dictionary; //children share the dictionary of the root (so there is only 1 instance per choice tree)
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
	public void setIn(CollectorUI ui)
	{
		ui.setChoice(this);
	}
	
	public String toString()
	{
		return "ChoiceField " + id + (value != null ? " (value: " + value + ")" : " (no value set)");
	}
	
	@Override
	protected IntegerColumn createColumn()
	{
		if(!isRoot())
			throw new IllegalStateException("createColumn() should only be called on a root ChoiceField object.");
		dictionary.initialise(this); //!!!
		if(dictionary.isEmpty())
		{	//no values set
			form.addWarning("noColumn was forced to true on ChoiceField " + getID() + " because it has no values.");
			noColumn = true; //!!!
			return null;
		}
		else
		{	//Create column:
			return new IntegerColumn(id, (optional != Optionalness.NEVER), 0, dictionary.size() - 1);
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
		if(value == null) //we don't need to check for empty String because those are replaced by null in the constructor
		{
			if(parent != null)
				return parent.getLowestAncestorWithValue(); //recursive call
			else
				return null; //in case there is no value all the way to the root
		}
		else
			return this; //return self
	}
	
	public void storeValue(Record record)
	{
		if(!isNoColumn() && isLeaf())
			((IntegerColumn) form.getColumnFor(root)).storeValue(record, Long.valueOf(dictionary.getIndex(this))); //this = the selected leaf
	}
	
	public ChoiceDictionary getDictionary()
	{
		return dictionary;
	}
	
	public static class ChoiceDictionary
	{
		
		/* HashMap which maps Choices that are both "valued" (i.e. with non-null value String) AND
		 * "selectable" (being either a leaf itself or the lowest "valued" ancestor of a "non-valued" leaf)
		 * into indexes, which are used to store the value (i.e. the choice made) of the ChoiceField tree. */
		private HashMap<ChoiceField, Integer> valuedToIdx;
		
		/* An (Array)List which allows choices to be looked up by index */
		private List<ChoiceField> indexed;
		
		public ChoiceDictionary()
		{
			valuedToIdx = new HashMap<ChoiceField, Integer>();
			indexed = new ArrayList<ChoiceField>();
		}
		
		/**
		 * <b>Note:</b> This method should only be called after the whole choice tree is parsed & constructed (i.e. from addColumns()).
		 */
		protected void initialise(ChoiceField root)
		{
			if(root.isRoot())
				traverse(root);
			else
				throw new IllegalArgumentException("ChoiceDictionary can only be initialised from the root choice.");
		}
	
		/**
		 * Recursive method which implements a depth-first traversal that finds all leaves and stores them or their lowest valued ancestor in the dictionary.
		 */
		private void traverse(ChoiceField choice)
		{
			if(choice.isLeaf())
			{
				ChoiceField valuedChoice = choice.getLowestAncestorWithValue();
				if(valuedChoice != null && !valuedToIdx.containsKey(valuedChoice))
				{
					valuedToIdx.put(valuedChoice, indexed.size());
					indexed.add(valuedChoice);
				}
			}
			else
			{
				for(ChoiceField child : choice.children) //Depth-first traversal
					traverse(child); //recursive call
			}
		}
		
		public boolean isEmpty()
		{
			return indexed.isEmpty();
		}
		
		public int size()
		{
			return indexed.size();
		}
		
		public int getIndex(ChoiceField choice)
		{
			Integer idx = valuedToIdx.get(choice.getLowestAncestorWithValue());
			if(idx != null)
				return idx.intValue();
			else
				return NON_SELECTABLE_CHOICE_INDEX;
		}
		
		public ChoiceField getChoice(int index)
		{
			return indexed.get(index);
		}
		
		public String toCSV(String separator)
		{
			StringBuffer bff = new StringBuffer();
			bff.append("INDEX" + separator + "VALUE" + separator + "IMG" + separator + "ID/PATH" + "\n");
			int idx = 0;
			for(ChoiceField choice : indexed)
				bff.append(idx++ + separator + choice.value + separator + choice.imageLogicalPath + separator + choice.id + "\n");
			return bff.toString();
		}
		
	}
	
}
