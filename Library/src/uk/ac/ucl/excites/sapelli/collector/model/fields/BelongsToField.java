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
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.xml.FormParser;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;

/**
 * Many-to-one relationship between forms.</br>
 * 	A N:1 relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
 * 	This means multiple records (N) of this form can relate (or better: "belong") to a single record (1) of the {@code relatedForm}.  
 * 
 * @author mstevens
 */
public class BelongsToField extends Relationship
{

	//STATICS -------------------------------------------------------
	static public final String PARAMETER_EDIT = "edit";
	static public final String PARAMETER_WAITING_FOR_RELATED_FORM = "watingForRelatedForm";
	
	// Dynamics------------------------------------------------------
	
	/**
	 * @param form
	 * @param id
	 */
	public BelongsToField(Form form, String id)
	{
		super(form, id);
		this.noColumn = false;
	}
	
	public void setRelatedForm(Form relatedForm)
	{
		if(!relatedForm.isProducesRecords())
			throw new IllegalArgumentException("The related form of a BelongsTo field must produce records!");
		super.setRelatedForm(relatedForm);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected ForeignKeyColumn createColumn(String name)
	{	
		return new ForeignKeyColumn(name, relatedForm.getSchema(), (optional != Optionalness.NEVER)); // (BelongsTo)
	}
	
	@Override
	public ForeignKeyColumn getColumn()
	{
		return (ForeignKeyColumn) super.getColumn();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(Controller controller, FieldParameters arguments, boolean withPage)
	{
		return controller.enterBelongsTo(this, arguments);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof BelongsToField)
			return super.equals(obj); // Relationship#equals(Object)
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Relationship#hashCode()
		hash = 31 * hash + FormParser.TAG_BELONGS_TO.hashCode();
		return hash;
	}
	
}
