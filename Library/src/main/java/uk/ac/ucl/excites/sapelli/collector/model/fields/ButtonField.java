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

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ButtonUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;

/**
 * A Field that represents an on-screen button, it can optionally have either Boolean- or DateTime column.
 * 
 * @author mstevens
 */
public class ButtonField extends Field
{
	
	// Statics --------------------------------------------
	static public enum ButtonColumnType
	{
		NONE,
		BOOLEAN,
		DATETIME
	}
	
	static public final String ID_PREFIX = "btn";
	static public final ButtonColumnType DEFAULT_COLUMN_TYPE = ButtonColumnType.NONE;
	
	// Dynamics -------------------------------------------
	private ButtonColumnType columnType;
	
	/**
	 * @param form
	 * @param id (may be null)
	 * @param caption (may not be null)
	 */
	public ButtonField(Form form, String id, String caption)
	{	
		super(form, GetID(id, form, ID_PREFIX, caption), CheckCaption(caption));
		setColumnType(DEFAULT_COLUMN_TYPE); // also sets noColumn
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#canJumpFromPage()
	 */
	@Override
	public boolean canJumpFromPage()
	{
		return true;
	}

	public void setColumnType(String columnTypeStr) throws IllegalArgumentException
	{
		setColumnType(ButtonColumnType.valueOf(columnTypeStr.toUpperCase()));
	}
	
	public void setColumnType(ButtonColumnType columnType)
	{
		this.columnType = columnType;
		this.noColumn = (this.columnType == ButtonColumnType.NONE); 
	}
	
	/**
	 * @return the columnType
	 */
	public ButtonColumnType getColumnType()
	{
		return columnType;
	}

	public void setNoColumn(boolean noColumn)
	{
		this.noColumn = noColumn;
		if(noColumn)
			columnType = ButtonColumnType.NONE;
		else if(columnType == ButtonColumnType.NONE) // intended column type is (still) unknown
		{	// for now noColumn is only parsed on <Choice>s, but we already post this warning in case this would change in the future:
			form.addWarning("Attribute 'noColumn=\"false\"' on <Button> is ambiguous (and therefore ignored), please use 'column=\"boolean\"' or 'column=\"datetime\"' instead.");
			//throw new UnsupportedOperationException("setNoColumn(false) is unsupported on ButtonFields, use setColumnType(String) or setColumnType(ButtonColumnType) instead.");
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected Column<?> createColumn(String name)
	{
		switch(columnType)
		{
			case BOOLEAN :
				return new BooleanColumn(name, form.getColumnOptionalityAdvisor().getColumnOptionality(this));
			case DATETIME :
				return TimeStampColumn.Century21NoMS(name, form.getColumnOptionalityAdvisor().getColumnOptionality(this), true);
			case NONE :
			default :
				return null;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterButtonField(this, arguments, withPage);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(UI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> ButtonUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createButtonUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof ButtonField)
		{
			ButtonField that = (ButtonField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.columnType == that.columnType;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + columnType.ordinal();
		return hash;
	}

}
