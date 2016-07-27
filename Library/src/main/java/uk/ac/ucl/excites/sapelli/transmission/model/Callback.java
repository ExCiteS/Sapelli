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

package uk.ac.ucl.excites.sapelli.transmission.model;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * @author mstevens
 *
 */
public class Callback
{
	
	protected void store(final Transmission<?> transmissionToSave)
	{
		if(transmissionToSave == null)
			return;
		try
		{
			transmissionToSave.client.transmissionStoreHandle.executeNoDBEx(new StoreHandle.StoreOperation<TransmissionStore, Exception>()
			{
				@Override
				public void execute(TransmissionStore store) throws Exception
				{
					store.store(transmissionToSave);
				}
			});
		}
		catch(Exception e)
		{
			transmissionToSave.client.logError("Error upon storing transmission from " + getClass().getSimpleName(), e);
		}
	}

}
