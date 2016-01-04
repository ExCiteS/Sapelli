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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;

/**
 * @author mstevens
 *
 */
public final class GeoKeyTasks
{

	private GeoKeyTasks() {}
	
	static public void VerifyServerAndAccount(final BaseActivity owner, final GeoKeyAccount account, final VerificationCallback callback)
	{
		// Verify account:
		if(!DeviceControl.isOnline(owner))
			// We are not online...
			callback.noInternet();
		else
			// We are online, perform actual verification:
			new AsyncTaskWithWaitingDialog<BaseActivity, GeoKeyAccount, Boolean>(owner, R.string.verifyingServerAndAccount)
			{
				@Override
				protected Boolean runInBackground(GeoKeyAccount... params)
				{
					return new GeoKeySapelliSession(getContext(), params[0]).login();
				}
	
				@Override
				protected void onPostExecute(Boolean result)
				{
					// Dismiss waiting dialog:
					super.onPostExecute(result);
					// Deal with result:
					if(result == null || !result)
						callback.invalidAccount(); // TODO server check
					else
						callback.validAccount();
				}
			}.execute(account);
	}
	
	public interface VerificationCallback
	{
		
		public void noInternet();
		
		public void invalidServer();
		
		public void invalidAccount();
		
		public void validAccount();
		
	}
	
}
