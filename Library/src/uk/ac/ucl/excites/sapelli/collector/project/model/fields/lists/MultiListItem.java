package uk.ac.ucl.excites.sapelli.collector.project.model.fields.lists;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.project.model.dictionary.DictionaryItem;

public class MultiListItem implements DictionaryItem
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
