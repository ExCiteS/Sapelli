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

package uk.ac.ucl.excites.sapelli.relay.util;

import android.os.Handler;

/**
 * Class for updating the UI
 * 
 * @author Michalis Vitos
 * 
 */
public class UIHelper
{
	private Handler handler = new Handler();
	private Runnable status;

	private int UPDATE_INTERVAL = 2000; // 2 seconds

	public UIHelper(final Runnable updater)
	{
		status = new Runnable()
		{
			@Override
			public void run()
			{
				// Run the passed runnable
				updater.run();
				// Re-run it after the update interval
				handler.postDelayed(this, UPDATE_INTERVAL);
			}
		};
	}

	public UIHelper(Runnable updater, int interval)
	{
		this(updater);
		UPDATE_INTERVAL = interval;
	}

	public void startUpdates()
	{
		status.run();
	}

	public void stopUpdates()
	{
		handler.removeCallbacks(status);
	}
}
