package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.DictionaryItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.util.CollectionUtils;


/**
 * Each ChoiceField represents a node in a decision tree. The whole of such a tree (starting with the root) describes the possible values the field (stored as an IntegerColumn) can take.
 * 
 * @author mstevens
 */
public class ChoiceField extends Field implements DictionaryItem
{
	
	static public final int DEFAULT_NUM_COLS = 1;
	static public final int DEFAULT_NUM_ROWS = 2;
	static public final String DEFAULT_ALT_TEXT = "?";
	static public final boolean DEFAULT_CROSSED = false;
	static public final String DEFAULT_CROSS_COLOR = "#A5FF0000"; // Red with 65% alpha
	
	private ChoiceField parent;
	private ChoiceField root;
	private List<ChoiceField> children;
	private String imageRelativePath;
	private int cols;
	private int rows;
	private String altText;
	private boolean crossed;
	private String crossColor;
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
	 * @return the imageRelativePath
	 */
	public String getImageRelativePath()
	{
		return imageRelativePath;
	}

	/**
	 * @param imageRelativePath the imageRelativePath to set
	 */
	public void setImageRelativePath(String imageRelativePath)
	{
		this.imageRelativePath = imageRelativePath;
	}
	
	public boolean hasImage()
	{
		return imageRelativePath != null;
	}

	/**
	 * @return the altText
	 */
	public String getAltText()
	{
		if(altText != null)
			return altText;
		if(imageRelativePath != null)
			return imageRelativePath;
		if(value != null)
			return value;
		return DEFAULT_ALT_TEXT;
	}

	/**
	 * @param altText the altText to set
	 */
	public void setAltText(String altText)
	{
		this.altText = altText;
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
	
	/**
	 * @return the crossed
	 */
	public boolean isCrossed()
	{
		return crossed;
	}

	/**
	 * @param crossed the crossed to set
	 */
	public void setCrossed(boolean crossed)
	{
		this.crossed = crossed;
	}

	/**
	 * @return the crossColor
	 */
	public String getCrossColor()
	{
		return crossColor;
	}

	/**
	 * @param crossColor the crossColor to set
	 */
	public void setCrossColor(String crossColor)
	{
		this.crossColor = crossColor;
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
	public List<File> getFiles(Project project)
	{
		List<File> paths = new ArrayList<File>();
		if(hasImage())
			CollectionUtils.addIgnoreNull(paths, project.getImageFile(imageRelativePath));
		for(ChoiceField child : children)
			CollectionUtils.addAllIgnoreNull(paths, child.getFiles(project));
		return paths;
	}
	
	public String toString()
	{
		return toString(false);
	}
	
	public String toString(boolean printValue)
	{
		if(printValue)
			return "ChoiceField " + id + (value != null ? " (value: " + value + ")" : " (no value set)");
		else
			return id;
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
			((IntegerColumn) form.getColumnFor(root)).storeValue(record, Long.valueOf(dictionary.lookupIndex(this))); //this = the selected leaf
	}
	
	public ChoiceDictionary getDictionary()
	{
		return dictionary;
	}
	
	@Override
	public boolean enter(Controller controller)
	{
		return controller.enterChoiceField(this);
	}
	
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createChoiceUI(this);
	}
	
	@Override
	public List<String> getDocExtras()
	{
		return Arrays.asList(this.imageRelativePath, this.id);
	}
	
	/**
	 * A Dictionary for ChoiceFields.
	 * 
	 * Holds a (Hash)Map (itemToIndex) which maps ChoiceFields that are both "valued" (i.e. with non-null value String) AND
	 * "selectable" (being either a leaf itself or the lowest "valued" ancestor of a "non-valued" leaf) to indexes,
	 * which are used to store the value (i.e. the choice made) of the ChoiceField tree.
	 * 
	 * Also holds an (Array)List which allows choices to be looked up by index.
	 *
	 * @author mstevens
	 */
	public static class ChoiceDictionary extends Dictionary<ChoiceField>
	{
		
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
				if(valuedChoice != null && !itemToIndex.containsKey(valuedChoice))
				{
					itemToIndex.put(valuedChoice, indexed.size());
					indexed.add(valuedChoice);
				}
			}
			else
			{
				for(ChoiceField child : choice.children) //Depth-first traversal
					traverse(child); //recursive call
			}
		}
		
		@Override
		protected List<String> getDocHeaders()
		{
			List<String> hdrs = super.getDocHeaders();
			hdrs.add("IMG");
			hdrs.add("ID/PATH");
			return hdrs;
		}
				
	}
	
}
