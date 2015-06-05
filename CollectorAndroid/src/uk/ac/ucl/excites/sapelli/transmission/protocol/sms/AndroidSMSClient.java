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

package uk.ac.ucl.excites.sapelli.transmission.protocol.sms;

import java.util.ArrayList;
import java.util.Random;

import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import uk.ac.ucl.excites.sapelli.util.SMSStatusReport;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.telephony.SmsManager;
import android.util.Log;

public class AndroidSMSClient implements SMSClient
{
	
	private static final String TAG = "SMSSender";
	private static final String SMS_SENT = "SMS_SENT";
	private static final String SMS_DELIVERED = "SMS_DELIVERED";
	
	/**
	 * The choice of port might affect the size of the UDH header Android uses (5 or 7 bytes).
	 * <br/>
	 * The current value ({@value #SMS_PORT}) would require 16 bits, and therefore a 7-byte UDH, but it could be that Android always uses the 7 byte header, even for 8-bit ports.
	 * <br/><br/>
	 * 
	 * @see BinaryMessage#MAX_TOTAL_SIZE_BYTES
	 * @see BinaryMessage
	 * @see <a href="http://en.wikipedia.org/wiki/User_Data_Header">User Data Header (UDH)</a>
	 */
	public static final short SMS_PORT = 2013;
	
	/**
	 * Start at random Message ID:
	 */
	private static int MESSAGE_ID = new Random().nextInt();
	
	private Context context;
	private SmsManager smsManager;
	
	public AndroidSMSClient(Context context)
	{
		this.context = context;
		this.smsManager = SmsManager.getDefault();
	}

	@Override
	public boolean send(SMSCorrespondent receiver, final TextMessage textSMS)
	{	
		// Increment message ID!:
		MESSAGE_ID++;
		//Try sending:
		try
		{
			Log.d(TAG, "Try sending Text SMS with length of " + textSMS.getContent().length() + " chars, and content: " + textSMS.getContent());
			if(textSMS.isMultiPart())
			{	// Send multiple SMSs:
				ArrayList<String> parts = smsManager.divideMessage(textSMS.getContent());
				ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
				ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();
				for(int p = 0; p < parts.size(); p++)
				{
					sentIntents.add(setupSentCallback(textSMS, MESSAGE_ID, p, parts.size()));
					deliveryIntents.add(setupDeliveredCallback(textSMS, MESSAGE_ID, p, parts.size()));
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
										SMS_PORT,
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
	
	private <M extends Message<M, C>, C> PendingIntent setupSentCallback(final M msg, final int messageID)
	{
		return setupSentCallback(msg, messageID, 1, 1);
	}
	
	private <M extends Message<M, C>, C> PendingIntent setupSentCallback(final M msg, final int messageID, final int part, final int numParts)
	{
		// Generate intentAction (to be used by both the PendingIntent and the Receiver):
		String intentAction = SMS_SENT + "_" + messageID + (numParts > 1 ? ("_" + part + "/" + numParts) : "");
		
		// Set-up receiver:
		context.registerReceiver(
			new BroadcastReceiver()
			{
				@Override
				public void onReceive(Context context, Intent intent)
				{
					// Unregister receiver (to avoid it being triggered more than once):
					context.unregisterReceiver(this);
					// Prepare log message:
					String msgDescription = "[SMS-ID: " + messageID +
											(numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "") +
											"; TRANSMISSION-ID: " + msg.getTransmission().getLocalID() +
											"; TRANSMISSION-PART: " + msg.getPartNumber() + "/" + msg.getTotalParts() + "]";
					// Handle result:
					switch(getResultCode())
					{
						case Activity.RESULT_OK:
							// Use call back to register send time of the message, and possibly that of the whole transmission, and save the msg/transmission to the TransmissionStore:
							msg.getTransmission().getSentCallback().onSent(msg);
							Log.i(TAG, "Sending " + msgDescription + ": success.");
							break;
						case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
							Log.i(TAG, "Sending " + msgDescription + ": generic failure.");
							break;
						case SmsManager.RESULT_ERROR_NO_SERVICE:
							Log.i(TAG, "Sending " + msgDescription + ": no service error.");
							break;
						case SmsManager.RESULT_ERROR_NULL_PDU:
							Log.i(TAG, "Sending " + msgDescription + ": null PDU error.");
							break;
						case SmsManager.RESULT_ERROR_RADIO_OFF:
							Log.i(TAG, "Sending " + msgDescription + ": radio off error.");
							break;
					}
				}
			},
			new IntentFilter(intentAction));
		
		//Return pending intent:
		return PendingIntent.getBroadcast(context, 0, new Intent(intentAction), PendingIntent.FLAG_ONE_SHOT);
	}
	
	private <M extends Message<M, C>, C> PendingIntent setupDeliveredCallback(final M msg, final int messageID)
	{
		return setupDeliveredCallback(msg, messageID, 1, 1);
	}
	
	private <M extends Message<M, C>, C> PendingIntent setupDeliveredCallback(final M msg, final int messageID, final int part, final int numParts)
	{
		// Generate intentAction (to be used by both the PendingIntent and the Receiver):
		String intentAction = SMS_DELIVERED + "_" + messageID + (numParts > 1 ? ("_" + part + "/" + numParts) : "");
		// Set-up receiver:
		context.registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				// Unregister receiver (to avoid it being triggered more than once):
				context.unregisterReceiver(this);
				// Prepare log message:
				String msgDescription = "[SMS-ID: " + messageID +
										(numParts > 1 ? ("; SMS-PART:" + part + "/" + numParts) : "") +
										"; TRANSMISSION-ID: " + msg.getTransmission().getLocalID() +
										"; TRANSMISSION-PART: " + msg.getPartNumber() + "/" + msg.getTotalParts() + "]";
				// Handle result:
				switch(getResultCode())
				{
					case Activity.RESULT_OK:
						// Try to parse pdu to check delivery success & get delivery time ("discharge time"):
						TimeStamp deliveredAt = TimeStamp.now(); // use current time as fall-back
						if(intent.hasExtra("pdu") && DeviceControl.isGSM(context)) // SMSStatusReport only works on GSM/3GPP phones (not on CDMA/3GPP2)
						{
							try
							{
								SMSStatusReport statusReport = new SMSStatusReport((byte[]) intent.getExtras().get("pdu"));
								//Log.d(TAG, "SMS-STATUS-REPORT: TP-SCTS=" + TimeUtils.getISOTimestamp(statusReport.getServiceCenterTimeStamp(), true) + "; TP-DT=" + TimeUtils.getISOTimestamp(statusReport.getDischargeTime(), true));
								if(!statusReport.isReceived())
								{	 // the report indicates the message was *not* received!
									Log.i(TAG, "Delivery " + msgDescription + ": failed (pdu indicates non-successful).");
									return;
								}
								deliveredAt = new TimeStamp(statusReport.getDischargeTime());
							}
							catch(Exception e)
							{
								Log.e(TAG, "Error parsing delivery report pdu", e);
							}
						}
						// Use call back to register delivery time of the message, and possibly that of the whole transmission, and save the msg/transmission to the TransmissionStore:
						msg.getTransmission().getSentCallback().onDelivered(msg, deliveredAt);
						Log.i(TAG, "Delivery " + msgDescription + ": success.");
						break;
					case Activity.RESULT_CANCELED:
						Log.i(TAG, "Delivery " + msgDescription + ": failure.");
						break;
				}
			}
		},
		new IntentFilter(intentAction));
		//Return pending intent:
		return PendingIntent.getBroadcast(context, 0, new Intent(intentAction), PendingIntent.FLAG_ONE_SHOT);
	}

}