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

import java.util.ArrayList;
import java.util.Random;

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsManager;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out.Helpers.SMSInfo;
import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSClient;

/**
 * @author Michalis Vitos, mstevens
 */
public class AndroidSMSClient implements SMSClient
{
	
	private static final String TAG = AndroidSMSClient.class.getSimpleName();
	
	/**
	 * Start at random Message ID:
	 */
	private static int MESSAGE_ID = new Random().nextInt();
	
	/**
	 * The choice of port might affect the size of the UDH header Android uses (5 or 7 bytes).
	 * <br/>
	 * The current value (= 2013) would require 16 bits, and therefore a 7-byte UDH, but it could be that Android always uses the 7 byte header, even for 8-bit ports.
	 * <br/><br/>
	 * 
	 * @see BinaryMessage#MAX_TOTAL_SIZE_BYTES
	 * @see BinaryMessage
	 * @see <a href="http://en.wikipedia.org/wiki/User_Data_Header">User Data Header (UDH)</a>
	 */
	private final short binarySMSPort;
	
	private Context context;
	private SmsManager smsManager;
	
	public AndroidSMSClient(Context context)
	{
		this.context = context;
		this.binarySMSPort = Integer.valueOf(context.getResources().getInteger(R.integer.binary_sms_port)).shortValue();
		this.smsManager = SmsManager.getDefault();
	}

	@Override
	public boolean send(SMSCorrespondent receiver, final TextMessage textSMS)
	{	
		// Increment message ID!:
		MESSAGE_ID++;
		// Try sending:
		try
		{
			Log.d(TAG, "Try sending Text SMS with length of " + textSMS.getContent().length() + " chars, and content: " + textSMS.getContent());
			if(textSMS.isMultiPart())
			{	// Send multiple SMSs:
				ArrayList<String> parts = smsManager.divideMessage(textSMS.getContent());
				ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
				ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();
				for(int mpNumber = 1; mpNumber < parts.size(); mpNumber++) // we use 1-based multi-part indexes (analogous to the Message part numbers) 
				{
					sentIntents.add(setupSentCallback(textSMS, MESSAGE_ID, mpNumber, parts.size()));
					deliveryIntents.add(setupDeliveredCallback(textSMS, MESSAGE_ID, mpNumber, parts.size()));
				}
				smsManager.sendMultipartTextMessage(receiver.getPhoneNumberDialable(),
													null,
													parts,
													sentIntents,
													deliveryIntents);			
			}
			else
			{	// Send single SMS:	
				smsManager.sendTextMessage(	receiver.getPhoneNumberDialable(),
											null,
											textSMS.getContent(),
											setupSentCallback(textSMS, MESSAGE_ID),
											setupDeliveredCallback(textSMS, MESSAGE_ID));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending " + (textSMS.isMultiPart() ? "multipart " : "")  + "text SMS to " + receiver.getPhoneNumberDialable());
			// Failure:
			return false;
		}
		// Success:
		return true;
	}

	@Override
	public boolean send(SMSCorrespondent receiver, final BinaryMessage binarySMS)
	{
		// Increment message ID!:
		MESSAGE_ID++;
		//Try sending:
		try
		{
			Log.d(TAG, "Sending binary SMS, content hash: " + BinaryHelpers.toHexadecimealString(Hashing.getMD5HashBytes(binarySMS.getContent())));
			smsManager.sendDataMessage(	receiver.getPhoneNumberDialable(),
										null,
										binarySMSPort,
										binarySMS.getContent(),
										setupSentCallback(binarySMS, MESSAGE_ID),
										setupDeliveredCallback(binarySMS, MESSAGE_ID));
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon sending binary SMS to " + receiver.getPhoneNumberDialable());
			// Failure:
			return false;
		}
		// Success:
		return true;
	}
	
	/**
	 * @param msg
	 * @param messageID
	 * @return
	 */
	private <M extends Message<M, C>, C> PendingIntent setupSentCallback(M msg, int messageID)
	{
		return setupSentCallback(msg, messageID, 1, 1);
	}
	
	/**
	 * @param msg
	 * @param smsID
	 * @param multiPartNumber a value from [1, multiPartTotal]
	 * @param multiPartTotal
	 * @return
	 */
	private <M extends Message<M, C>, C> PendingIntent setupSentCallback(M msg, int smsID, int multiPartNumber, int multiPartTotal)
	{
		return getPendingIntent(R.string.action_sms_sent, msg, smsID, multiPartNumber, multiPartTotal);
	}
	
	/**
	 * @param msg
	 * @param messageID
	 * @return
	 */
	private <M extends Message<M, C>, C> PendingIntent setupDeliveredCallback(M msg, int messageID)
	{
		return setupDeliveredCallback(msg, messageID, 1, 1);
	}
	
	/**
	 * @param msg
	 * @param smsID
	 * @param multiPartNumber a value from [1, multiPartTotal]
	 * @param multiPartTotal
	 * @return
	 */
	private <M extends Message<M, C>, C> PendingIntent setupDeliveredCallback(M msg, int smsID, int multiPartNumber, int multiPartTotal)
	{
		return getPendingIntent(R.string.action_sms_delivered, msg, smsID, multiPartNumber, multiPartTotal);
	}
	
	/**
	 * @param action
	 * @param msg
	 * @param smsID
	 * @param multiPartNumber a value from [1, multiPartTotal]
	 * @param multiPartTotal
	 * @return
	 */
	private <M extends Message<M, C>, C> PendingIntent getPendingIntent(int action, M msg, int smsID, int multiPartNumber, int multiPartTotal)
	{
		// Create intent
		Intent intent = new Intent(context.getResources().getString(action));
		new SMSInfo(msg, smsID, multiPartNumber, multiPartTotal).setIntentExtras(intent);

		// Return pending intent:
		return PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_ONE_SHOT);
	}

}