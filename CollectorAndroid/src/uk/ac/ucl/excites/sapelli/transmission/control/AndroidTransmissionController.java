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

package uk.ac.ucl.excites.sapelli.transmission.control;

import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStoreProvider;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.sender.gsm.SMSSender;
import android.content.Context;

/**
 * 
 * @author benelliott
 */
public class AndroidTransmissionController extends TransmissionController
{

	private Context context;
	private SMSSender smsSender;
	
	public AndroidTransmissionController(TransmissionClient transmissionClient, TransmissionStoreProvider transmissionStoreProvider, Context context) throws Exception
	{
		super(transmissionClient, transmissionStoreProvider);
		this.context = context;
	}

	@Override
	public SMSClient getSMSService()
	{
		if (smsSender == null)
			smsSender = new SMSSender(context);
		return smsSender;
	}

	@Override
	public HTTPClient getHTTPClient()
	{
		// TODO Auto-generated method stub
		return null;
	}

}
