/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.PageUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * A Page of a {@link Form}.
 * 
 * @author mstevens
 */
public class Page extends Field
{
	
	private final List<Field> fields;
	private final List<Trigger> triggers;

	/**
	 * Create a	new page
	 * Note: pages never have captions, form designers should user labels for that.
	 * 
	 * @param form
	 * @param id
	 */
	public Page(Form form, String id)
	{
		super(form, id);
		fields = new ArrayList<Field>();
		triggers = new ArrayList<Trigger>();
		noColumn = true; // Pages never have columns of their own
	}
	
	/**
	 * @param noColumn the noColumn to set
	 */
	public void setNoColumn(boolean noColumn)
	{
		// Ignore! Pages never have columns of their own.
	}
	
	public void addField(Field field)
	{
		if(field == null)
			throw new NullPointerException("Cannot add a null field object to a Page");
		
		fields.add(field);
		
		// Make child field "jump back" to the page, unless it is allowed to jump elsewhere:
		if(!field.canJumpFromPage())
			field.setJump(this);
	}

	public List<Field> getFields()
	{
		return fields;
	}
	
	public void addTrigger(Trigger trigger)
	{
		triggers.add(trigger);
	}

	/**
	 * @return the triggers
	 */
	public List<Trigger> getTriggers()
	{
		return triggers;
	}

	/**
	 * Overrides method of Field to ensure that the columns of fields contained
	 * by this Page get created and added to the Schema of the Form, even though
	 * the Page does not have a column of its own.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#addColumnTo(java.util.List)
	 */
	@Override
	protected void addColumnTo(List<Column<?>> columns)
	{
		for(Field f : fields)
			/* No need to call Field#isNoColumn() here, Field#getColumn() will return null in
			 * case of fields with noColumn=true, but these are filtered out by addIgnoreNull(): */
			CollectionUtils.addIgnoreNull(columns, f.getColumn());
	}
	
	@Override
	public Column<?> getColumn()
	{
		throw new UnsupportedOperationException("Page fields do not have a column of their own.");
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		throw new UnsupportedOperationException("Page fields do not have a column of their own.");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, boolean withPage)
	{
		if(withPage)
			throw new IllegalStateException("Pages cannot be nested!");
		return controller.enterPage(this);
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> PageUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createPageUI(this);
	}
	
}
