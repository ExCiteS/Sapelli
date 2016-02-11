/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
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
		field.setPage(this); // make the field know the page which contains it
		
		// Make child field "jump back" to the page, unless it is allowed to jump elsewhere:
		if(!field.canJumpFromPage())
			field.setJump(this);
	}

	public List<Field> getFields()
	{
		return fields;
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
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected Column<?> createColumn(String name)
	{
		throw new UnsupportedOperationException("Page fields do not have a column of their own.");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		if(withPage)
			throw new IllegalStateException("Pages cannot be nested!");
		return visitor.enterPage(this, arguments);
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> PageUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createPageUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof Page)
		{
			Page that = (Page) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.fields.equals(that.fields) &&
					this.getTriggers().equals(that.getTriggers());
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + fields.hashCode();
		hash = 31 * hash + (triggers == null ? 0 : triggers.hashCode());
		return hash;
	}
	
}
