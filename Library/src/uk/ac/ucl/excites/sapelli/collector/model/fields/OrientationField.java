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
import uk.ac.ucl.excites.sapelli.collector.ui.fields.OrientationUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;

/**
 * @author mstevens
 *
 */
public class OrientationField extends Field
{
	
	//Statics
	static public final boolean DEFAULT_STORE_AZIMUTH = true; 
	static public final boolean DEFAULT_STORE_PITCH = true;
	static public final boolean DEFAULT_STORE_ROLL = true;
	
	static public final String ID_PREFIX = "orient";
	
	//Dynamics:
	private boolean storeAzimuth = DEFAULT_STORE_AZIMUTH;
	private boolean storePitch = DEFAULT_STORE_PITCH;
	private boolean storeRoll = DEFAULT_STORE_ROLL;
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, may be null (but not recommended)
	 * @param caption the caption of the field, may be null
	 */
	public OrientationField(Form form, String id, String caption)
	{
		super(form, GetID(id, form, ID_PREFIX, caption), caption);
	}

	/**
	 * @return the storeAzimuth
	 */
	public boolean isStoreAzimuth()
	{
		return storeAzimuth;
	}

	/**
	 * @param storeAzimuth the storeAzimuth to set
	 */
	public void setStoreAzimuth(boolean storeAzimuth)
	{
		this.storeAzimuth = storeAzimuth;
	}

	/**
	 * @return the storePitch
	 */
	public boolean isStorePitch()
	{
		return storePitch;
	}

	/**
	 * @param storePitch the storePitch to set
	 */
	public void setStorePitch(boolean storePitch)
	{
		this.storePitch = storePitch;
	}

	/**
	 * @return the storeRoll
	 */
	public boolean isStoreRoll()
	{
		return storeRoll;
	}

	/**
	 * @param storeRoll the storeRoll to set
	 */
	public void setStoreRoll(boolean storeRoll)
	{
		this.storeRoll = storeRoll;
	}

	@Override
	protected OrientationColumn createColumn(String name)
	{
		return new OrientationColumn(name, form.getColumnOptionalityAdvisor().getColumnOptionality(this), storeAzimuth, storePitch, storeRoll);
	}

	public void storeOrientation(Record record, Orientation orientation)
	{
		((OrientationColumn) form.getColumnFor(this)).storeValue(record, orientation);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterOrientationField(this, arguments, withPage);
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> OrientationUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createOrientationUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof OrientationField)
		{
			OrientationField that = (OrientationField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.storeAzimuth == that.storeAzimuth &&
					this.storePitch == that.storePitch &&
					this.storeRoll == that.storeRoll;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (storeAzimuth ? 0 : 1);
		hash = 31 * hash + (storePitch ? 0 : 1);
		hash = 31 * hash + (storeRoll ? 0 : 1);
		return hash;
	}

}
