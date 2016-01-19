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
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;

/**
 * @author mstevens
 *
 */
public final class GeoKeyTasks
{
	
	static public enum CheckResult
	{
		NoInternet,
		InvalidServer,
		InvalidAccount,
		NoSuchProject,
		OK
	};

	private GeoKeyTasks() {}
	
	/**
	 * @param owner
	 * @param account
	 * @param project - may be null
	 * @param callback
	 */
	static public void CheckServer(final BaseActivity owner, final GeoKeyAccount account, final Project project, final CheckCallback callback)
	{
		new AsyncTaskWithWaitingDialog<BaseActivity, Void, CheckResult>(owner, R.string.verifyingServerAndAccount)
		{
			@Override
			protected CheckResult runInBackground(Void... params)
			{
				if(!DeviceControl.isOnline(owner))
					return CheckResult.NoInternet;
				
				// We are online, perform actual checks:
				AndroidGeoKeyClient client = new AndroidGeoKeyClient(owner.getCollectorApp());
				
				//	Check if server is reachable and run the right GeoKey & extension versions:
				if(!client.verifyServer(account))
					return CheckResult.InvalidServer;
				
				//	Check if user can login:
				if(!client.login(account))
					return CheckResult.InvalidAccount;

				// Check if given project exists on server:
				if(project != null && client.doesServerHaveProjectForContribution(project))
					return CheckResult.NoSuchProject;
				
				// All OK!:
				return CheckResult.OK;
			}

			@Override
			protected void onPostExecute(CheckResult result)
			{
				// Dismiss waiting dialog:
				super.onPostExecute(result);
				// Deal with result:
				if(callback != null)
					callback.checkDone(result);
			}
		}.execute();
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public interface CheckCallback
	{
		
		public void checkDone(CheckResult result);
		
	}
	
}
