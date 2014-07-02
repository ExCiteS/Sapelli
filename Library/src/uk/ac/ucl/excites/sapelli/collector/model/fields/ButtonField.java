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

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ButtonUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;

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
	static public final ButtonColumnType DEFAULT_COLUMN = ButtonColumnType.NONE;
	
	// Dynamics -------------------------------------------
	private ButtonColumnType columnType;
	
	/**
	 * @param form
	 * @param id
	 * @param caption
	 */
	public ButtonField(Form form, String id, String caption)
	{	
		super(form, (id == null || id.isEmpty() ? captionToID(ID_PREFIX, form, caption) : id), caption);
		setColumnType(DEFAULT_COLUMN);
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
		else if(columnType == ButtonColumnType.NONE) // intended column type is still unknown
		{
			throw new UnsupportedOperationException("setNoColumn(false) is unsupported on ButtonFields, use setColumnType(String) or setColumnType(ButtonColumnType) instead.");
			// If we were to start parsing noColumn for more than just ChoiceFields (as currently), then this warning would be nicer than the exception above:
			//form.addWarning("Attribute 'noColumn=\"false\"' is ambiguous (and therefore ignored) on <Button>s, please use 'column=\"boolean\"' or 'column=\"datetime\"' instead."); 
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		switch(columnType)
		{
			case BOOLEAN : return new BooleanColumn(id, optional != Optionalness.NEVER);
			case DATETIME : return TimeStampColumn.Century21NoMS(id, optional != Optionalness.NEVER, true);
			/* case NONE */ default : return null;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, FieldParameters arguments, boolean withPage)
	{
		return true;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(UI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> ButtonUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createButtonUI(this);
	}

}
