/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

import uk.ac.ucl.excites.sapelli.collector.model.Form;

/**
 * Abstract super class for PhotoField & VideoField.
 * 
 * @author mstevens
 */
public abstract class CameraField extends MediaField
{

	protected boolean useFrontFacingCamera;
	
	/**
	 * @param form
	 * @param id
	 * @param caption
	 */
	/**
	 * @param form
	 * @param id
	 * @param caption
	 * @param useNativeApp default {@link #useNativeApp} value
	 * @param useFrontFacingCamera default {@link #useFrontFacingCamera} value
	 */
	public CameraField(Form form, String id, String caption, boolean useNativeApp, boolean useFrontFacingCamera)
	{
		super(form, id, caption, useNativeApp);
		this.useFrontFacingCamera = useFrontFacingCamera;
	}
	
	/**
	 * @return the useFrontFacingCamera
	 */
	public boolean isUseFrontFacingCamera()
	{
		return useFrontFacingCamera;
	}

	/**
	 * @param useFrontFacingCamera the useFrontFacingCamera to set
	 */
	public void setUseFrontFacingCamera(boolean useFrontFacingCamera)
	{
		this.useFrontFacingCamera = useFrontFacingCamera;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof CameraField)
		{
			CameraField that = (CameraField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					this.useFrontFacingCamera == that.useFrontFacingCamera;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + (useFrontFacingCamera ? 0 : 1);
		return hash;
	}

}
