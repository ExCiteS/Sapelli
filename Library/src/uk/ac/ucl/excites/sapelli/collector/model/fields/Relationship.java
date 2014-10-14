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

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;

/**
 * Field that represents relationship with another Form
 * <p>
 * We envision the following different type (to be implemented by subclasses):
 * </p><p> 
 * "LinksTo": A "jump" link between forms, implemented in {@link LinksToField}</br>
 *	The relationship between the forms is purely "navigational" and there is no stored association between their records.
 *	In this case the Relation field merely provides a "passage way" through which navigation to the other form is possible.
 *	An "intra-form" jump to the relation field will automatically result in a subsequent "inter-form" jump to the {@code relatedForm}. 
 *</p><p>
 * One-to-one: TODO not yet implemented</br>
 *	A 1:1 relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
 *	The consequence is that a new record of the {@code relatedForm} will be created for every instance of this form (unless the relationship is optional). 
 * </p><p>
 * Many-to-one: Implemented in {@link BelongsToField}.
 * 	A N:1 relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
 * 	This means multiple records (N) of this form can relate (or better: "belong") to a single record (1) of the {@code relatedForm}.  
 * </p><p>
 * Many-to-many: TODO not yet implemented</br>
 *	A N:M relationship between this form (holder of of the Relationship object and another {@code relatedForm}.
 *	This means multiple records (N) of this form can relate to (or better: "have and belong to") multiple records (M) of the {@code relatedForm}.
 *	Currently not implemented. Implementing this will require some kind of "cross table".
 * </p>
 * 
 * @author mstevens
 */
public abstract class Relationship extends UILessField
{

	//STATICS -------------------------------------------------------
	static public final boolean DEFAULT_HOLD_FOREIGN_RECORD = false;
	
	// Dynamics------------------------------------------------------
	protected Form relatedForm;
	protected boolean holdForeignRecord;
	protected final AndConstraint constraints;

	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 * @param type the Relationshio.Type
	 */
	public Relationship(Form form, String id)
	{
		super(form, id);
		constraints = new AndConstraint();
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn is unsupported on Relation fields. Whether or not they have columns is soley determined by their type.");
	}
	
	public void setRelatedForm(Form relatedForm)
	{
		if(this.relatedForm != null)
			throw new IllegalStateException("relatedForm cannot be changed once it has been set!");
		if(relatedForm == form)
			throw new IllegalArgumentException("A form cannot be related to itself!"); //TODO why not? e.g. person-person relationship
		this.relatedForm = relatedForm;
	}
	
	public void addConstraint(Constraint constraint)
	{
		constraints.addConstraint(constraint);
	}

	/**
	 * @return the constraints
	 */
	public AndConstraint getConstraints()
	{
		return constraints;
	}
	
	/**
	 * @return the relatedForm
	 */
	public Form getRelatedForm()
	{
		return relatedForm;
	}

	/**
	 * @return the holdForeignRecord
	 */
	public boolean isHoldForeignRecord()
	{
		return holdForeignRecord;
	}

	/**
	 * @param holdForeignRecord the holdForeignRecord to set
	 */
	public void setHoldForeignRecord(boolean holdForeignRecord)
	{
		this.holdForeignRecord = holdForeignRecord;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#getFiles(uk.ac.ucl.excites.collector.project.model.Project)
	 */
	@Override
	public List<File> getFiles(Project project)
	{
		//TODO link button image?
		return null;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof Relationship)
		{
			Relationship that = (Relationship) obj;
			return	super.equals(that) && // Field#equals(Object)
					(this.relatedForm != null ? that.relatedForm != null && this.relatedForm.getID().equals(that.relatedForm.getID()) : that.relatedForm == null) && // do not use relatedForm itself to avoid potential endless loops!
					this.holdForeignRecord == that.holdForeignRecord &&
					this.constraints.equals(that.constraints);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (relatedForm == null ? 0 : relatedForm.getID().hashCode()); // do not use relatedForm itself to avoid potential endless loops!
		hash = 31 * hash + (holdForeignRecord ? 0 : 1);
		hash = 31 * hash + constraints.hashCode();
		return hash;
	}

}
