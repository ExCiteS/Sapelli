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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out.Helpers.SMSInfo;

/**
 * 
 * @author Michalis Vitos, mstevens
 */
public class OutgoingSMSBroadcastReceiver extends BroadcastReceiver
{
	
	/**
	 * All the actual work is done by the OutgoingSMSCallbackService (because we need access to the Application,
	 * which is not possible from a BroadcastReceiver). So all we do here is copy the intent extras and result code
	 * and start the right service class.
	 * 
	 * @see android.content.BroadcastReceiver#onReceive(android.content.Context, android.content.Intent)
	 */
	@Override
	public void onReceive(Context context, Intent intent)
	{
		Intent serviceIntent = new Intent(context, OutgoingSMSCallbackService.class);
		// set callback action:
		for(int actionID : Helpers.ACTIONS_IDS)
			if(intent.getAction().equals(context.getString(actionID)))
			{
				serviceIntent.putExtra(Helpers.EXTRA_CALLBACK_ACTION_ID, actionID);
				break;
			}
		// set message info:
		SMSInfo.FromIntent(intent).setIntentExtras(serviceIntent);
		// set result code:
		serviceIntent.putExtra(Helpers.EXTRA_RESULT_CODE, getResultCode());
		// set pdu:
		serviceIntent.putExtra(Helpers.EXTRA_PDU, intent.getExtras().getByteArray(Helpers.EXTRA_PDU));
		// launch the service using the intent:
		context.startService(serviceIntent);
	}
	
}