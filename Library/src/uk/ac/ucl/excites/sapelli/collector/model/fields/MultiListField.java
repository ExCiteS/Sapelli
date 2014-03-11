/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.DictionaryItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;

/**
 * Field that allows users to select a value from a hierarchy presented as multiple listboxes
 * 
 * @author mstevens
 */
public class MultiListField extends Field
{

	static public final String UNKNOWN_LABEL_PREFIX = "Level "; //TODO multilang
	static public final boolean DEFAULT_PRESELECT = true;
	static public final String LABEL_SEPARATOR = ";";
	
	private String[] labels;
	private MultiListItem itemsRoot;
	private boolean preSelect = DEFAULT_PRESELECT;
	private Dictionary<MultiListItem> values;
	
	/**
	 * @param form
	 * @param id
	 */
	public MultiListField(Form form, String id, String labels)
	{
		super(form, id);
		this.labels = labels.split(LABEL_SEPARATOR);
		this.itemsRoot = new MultiListItem(this);
		this.values = new Dictionary<MultiListItem>();
	}
	
	@Override
	public String getLabel()
	{
		return getLabel(0);
	}
	
	public String getLabel(int level)
	{
		if(level < 0)
			throw new IndexOutOfBoundsException("Level cannot be negative!");
		else if(level < labels.length)
			return labels[level];
		else
			return labels[0].isEmpty() ? "" : UNKNOWN_LABEL_PREFIX + level;
	}
	
	public String[] getLabels()
	{
		return labels;
	}
	
	/**
	 * @return the preSelect
	 */
	public boolean isPreSelect()
	{
		return preSelect;
	}

	/**
	 * @param preSelect the preSelect to set
	 */
	public void setPreSelect(boolean preSelect)
	{
		this.preSelect = preSelect;
	}

	public MultiListItem getItemsRoot()
	{
		return itemsRoot;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected IntegerColumn createColumn()
	{
		// Initialise dictionary:
		addLeaves(itemsRoot); // depth-first traversal
		// Column...
		if(values.isEmpty())
		{	//no values set
			form.addWarning("noColumn was forced to true on MultiListField " + getID() + " because it has no items.");
			noColumn = true; //!!!
			return null;
		}
		else
		{	//Create column:
			return new IntegerColumn(id, (optional != Optionalness.NEVER), 0, values.size() - 1);
		}
	}
	
	private void addLeaves(MultiListItem item)
	{
		if(item.isLeaf())
			values.addItem(item);
		else
			for(MultiListItem c : item.getChildren())
				addLeaves(c);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites.collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createMultiListUI(this);
	}
	
	/**
	 * A class representing items in the MultiListField
	 * 
	 * @author mstevens
	 */
	public static class MultiListItem implements DictionaryItem
	{
		
		static public final int NO_DEFAULT_ITEM_SET_IDX = -1; 

		static public MultiListItem GetDummyItem(MultiListField field, String value)
		{
			MultiListItem dummy = new MultiListItem(field);
			dummy.value = value;
			return dummy;
		}
		
		private MultiListField field;
		private MultiListItem parent;
		
		private String value;
		
		private List<MultiListItem> children = new ArrayList<MultiListItem>();;
		private int defaultChildIdx = NO_DEFAULT_ITEM_SET_IDX;

		/**
		 * Only for root item held directly by MultiListField
		 * 
		 * @param field
		 */
		/*package*/ MultiListItem(MultiListField field)
		{
			this.field = field;
			// parent & value stay null
			// children list initialised above
		}
		
		public MultiListItem(MultiListItem parent, String value)
		{
			if(parent == null)
				throw new IllegalArgumentException("Parent cannot be null");
			this.parent = parent;
			parent.addChild(this); //!!!
			this.field = parent.field;
			this.value = value;
			// children list initialised above
		}
		
		/**
		 * @return the value
		 */
		public String getValue()
		{
			return value;
		}
		
		public String toString()
		{
			return value;
		}
		
		public void addChild(MultiListItem child)
		{
			children.add(child);
		}
		
		/**
		 * @return the field
		 */
		public MultiListField getField()
		{
			return field;
		}

		/**
		 * @return the parent
		 */
		public MultiListItem getParent()
		{
			return parent;
		}
		
		public boolean isRoot()
		{
			return parent == null;
		}
		
		/**
		 * @return the children
		 */
		public List<MultiListItem> getChildren()
		{
			return children;
		}
		
		public boolean isLeaf()
		{
			return children.isEmpty();
		}
		
		/**
		 * @return the defaultChild
		 */
		public MultiListItem getDefaultChild()
		{
			if(defaultChildIdx == NO_DEFAULT_ITEM_SET_IDX)
				return null;
			else
				return children.get(defaultChildIdx);
		}
		
		/**
		 * @return the index of the defaultChild
		 */
		public int getDefaultChildIndex()
		{
			return defaultChildIdx;
		}

		/**
		 * @param defaultChild the defaultChild to set
		 */
		public void setDefaultChild(MultiListItem defaultChild)
		{
			int idx = children.indexOf(defaultChild);
			if(idx == -1)
				throw new IllegalArgumentException("Unknown child: " + defaultChild.toString());
			this.defaultChildIdx = idx;
		}

		@Override
		public List<String> getDocExtras()
		{
			return null;
		}

	}
	
}
