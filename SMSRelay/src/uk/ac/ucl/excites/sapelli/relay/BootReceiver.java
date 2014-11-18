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

package uk.ac.ucl.excites.sapelli.relay;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * A Boot Receiver that auto starts the Relay on device boot
 * 
 * @author Michalis Vitos
 * 
 */
public class BootReceiver extends BroadcastReceiver
{

	@Override
	public void onReceive(Context context, Intent intent)
	{

		Intent service = new Intent(context, BackgroundService.class);
		PendingIntent pendingIntent = PendingIntent.getService(context, 0, service, 0);

		// Set up a calendar 2 minutes from now
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(System.currentTimeMillis());
		calendar.add(Calendar.MINUTE, 2);

		// Schedule the alarm!
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		alarmManager.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(), pendingIntent);
	}

}
