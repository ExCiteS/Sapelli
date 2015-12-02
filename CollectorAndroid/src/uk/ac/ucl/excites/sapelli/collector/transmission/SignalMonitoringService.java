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

	@Override
	public void onStart(Intent intent, int startId)
	{
		super.onStart(intent, startId);
		
		signalMonitor = SignalMonitor.Start(getApplicationContext());
	}

	@Override
	public void onDestroy()
	{
		if(signalMonitor != null)
			signalMonitor.stop();
		
		super.onDestroy();
	}

}