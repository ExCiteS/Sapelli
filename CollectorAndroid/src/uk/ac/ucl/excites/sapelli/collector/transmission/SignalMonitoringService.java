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

package uk.ac.ucl.excites.sapelli.collector.transmission;

import android.app.IntentService;
import android.content.Intent;
import uk.ac.ucl.excites.sapelli.shared.util.android.SignalMonitor;

/**
 * @author mstevens
 *
 */
public abstract class SignalMonitoringService extends IntentService
{

	protected SignalMonitor signalMonitor;

	public SignalMonitoringService(String name)
	{
		super(name);
	}

	/**
	 * This is the old onStart method that will be called on pre-2.0 Android.
	 * On 2.0 or later onStartCommand() is used instead. 
	 * 
	 * @see android.app.IntentService#onStart(android.content.Intent, int)
	 */
	@Override
	public void onStart(Intent intent, int startId)
	{
		startMonitoring();
		super.onStart(intent, startId);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		startMonitoring();
	    return super.onStartCommand(intent, flags, startId);
	}
	
	protected void startMonitoring()
	{
		if(signalMonitor == null)
			signalMonitor = SignalMonitor.Start(getApplicationContext());
	}
	
	protected void stopSignalMonitoring()
	{
		if(signalMonitor != null)
		{
			signalMonitor.stop();
			signalMonitor = null;
		}
	}

	@Override
	public void onDestroy()
	{
		stopSignalMonitoring();
		
		super.onDestroy();
	}

}