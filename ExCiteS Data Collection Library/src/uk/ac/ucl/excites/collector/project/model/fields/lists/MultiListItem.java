package uk.ac.ucl.excites.collector.project.model.fields.lists;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.dictionary.DictionaryItem;

public class MultiListItem implements DictionaryItem
{

	private MultiListField field;
	private MultiListItem parent;
	private int level;
	
	private String value;
	
	private List<MultiListItem> children = new ArrayList<MultiListItem>();;
	private MultiListItem defaultChild;

	/**
	 * Only for root item held directly by MultiListField
	 * 
	 * @param field
	 */
	/*package*/ MultiListItem(MultiListField field)
	{
		this.field = field;
		this.level = 0;
		// parent & value stay null
		// children initialised above
	}
	
	public MultiListItem(MultiListItem parent, String value)
	{
		if(parent == null)
			throw new IllegalArgumentException("Parent cannot be null");
		this.parent = parent;
		parent.addChild(this);
		this.level = parent.level++;
		this.field = parent.field;
		this.value = value;
		// children initialised above
	}
	
	/**
	 * @return the value
	 */
	public String getValue()
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

	public int getLevel()
	{
		return level;
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
		return defaultChild;
	}

	/**
	 * @param defaultChild the defaultChild to set
	 */
	public void setDefaultChild(MultiListItem defaultChild)
	{
		this.defaultChild = defaultChild;
	}

	@Override
	public List<String> getDocExtras()
	{
		return null;
	}
	
}
