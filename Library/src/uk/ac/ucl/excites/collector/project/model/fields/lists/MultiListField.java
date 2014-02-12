/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields.lists;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.dictionary.Dictionary;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.IntegerColumn;

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
			return UNKNOWN_LABEL_PREFIX + level;
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
		return controller.enterMultiListField(this);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createMultiListUI(this);
	}

}
