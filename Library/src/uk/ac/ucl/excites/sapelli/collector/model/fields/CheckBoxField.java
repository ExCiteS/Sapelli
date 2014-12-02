/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.CheckBoxUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;

/**
 * @author Julia
 * 
 */
public class CheckBoxField extends Field
{

	static public final String ID_PREFIX = "chbx";
	
	static public final boolean DEFAULT_INITIAL_VALUE = false; // not ticked by default
	
	private boolean initialValue;

	/**
	 * @param form
	 * @param id the id of the field, may be null (but not recommended)
	 * @param caption the caption of the field, may be null (in which case the id is used as the caption)
	 */
	public CheckBoxField(Form form, String id, String caption)
	{
		super(	form,
				GetID(id, form, ID_PREFIX, caption),
				GetCaption(id, caption));
		this.initialValue = DEFAULT_INITIAL_VALUE;
	}

	/**
	 * @return the initialValue
	 */
	public boolean getInitialValue()
	{
		return initialValue;
	}
	
	/**
	 * @param initialValue the initialValue to set
	 */
	public void setInitialValue(boolean initialValue)
	{
		this.initialValue = initialValue;
	}

	@Override
	public BooleanColumn getColumn()
	{
		return (BooleanColumn) super.getColumn();
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected Column<?> createColumn(String name)
	{
		return new BooleanColumn(name, form.getColumnOptionalityAdvisor().getColumnOptionality(this));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterCheckboxField(this, arguments, withPage);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> CheckBoxUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createCheckBoxFieldUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof CheckBoxField)
		{
			CheckBoxField that = (CheckBoxField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.initialValue == that.initialValue;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (initialValue ? 0 : 1);
		return hash;
	}

}
