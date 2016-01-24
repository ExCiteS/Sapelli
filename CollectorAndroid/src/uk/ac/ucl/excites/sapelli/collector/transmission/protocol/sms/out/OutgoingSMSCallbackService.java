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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out;

import android.app.Activity;
import android.app.IntentService;
import android.content.Intent;
import android.telephony.SmsManager;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out.Helpers.SMSInfo;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.util.SMSStatusReport;

/**
 * IntentService which handles callbacks about the sending or delivery of SMS messages.
 * 
 * @author Michalis Vitos, mstevens
 */
public class OutgoingSMSCallbackService extends IntentService
{
	
	// STATIC -------------------------------------------------------
	static private final String TAG = OutgoingSMSCallbackService.class.getSimpleName();
	
	// DYNAMIC ------------------------------------------------------
	private CollectorApp app;

	public OutgoingSMSCallbackService()
	{
		super(TAG);
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		app = (CollectorApp) getApplication();
	}

	/**
	 * Called when the service is started through an intent, which should also contain SMS messages to be read by the service.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		try
		{
			final SMSInfo smsInfo = SMSInfo.FromIntent(intent);
			if(smsInfo.multiPartNumber == smsInfo.multiPartTotal)
			{
				SMSTransmission<?> smsT = app.collectorClient.transmissionStoreHandle.executeWithReturnNoEx(new StoreHandle.StoreOperationWithReturnNoException<TransmissionStore, SMSTransmission<?>>()
				{
					@Override
					public SMSTransmission<?> execute(TransmissionStore store)
					{
						return store.retrieveSMSTransmission(false, smsInfo.transmissionLocalID, smsInfo.transmissionBinary, smsInfo.transmissionNumberOfParts);
					}
				});
				if(smsT == null)
					return;
				getHandler(intent.getIntExtra(Helpers.EXTRA_CALLBACK_ACTION_ID, Integer.MIN_VALUE)).handle(
					smsInfo.smsID,
					smsT.getPart(smsInfo.transmissionPartNumber),
					intent.getIntExtra(Helpers.EXTRA_RESULT_CODE, Integer.MIN_VALUE),
					intent.getByteArrayExtra(Helpers.EXTRA_PDU));
			}
			else
				throw new UnsupportedOperationException("There is no support (yet) for keeping track of sending/delivery of multipart SMS messages.");
		}
		catch(Exception e)
		{
			Log.e(TAG, "Exception upon handling outgoing SMS status update.", e);
		}
	}
	
	private Handler getHandler(int actionID)
	{
		switch(actionID)
		{
			case R.string.action_sms_sent :
				return new SentHandler();
			case R.string.action_sms_delivered :
				return new DeliveredHandler();
			default :
				throw new IllegalArgumentException("Unknown action ID (" + actionID + ")!");
		}
	}
	
	private String getMsgDescription(int smsID, Message<?, ?> msg)
	{
		return	"[SMS-ID: " + smsID +
				(msg.getTotalParts() > 1 ? ("; SMS-PART:" + msg.getPartNumber() + "/" + msg.getTotalParts()) : "") +
				"; TRANSMISSION-ID: " + msg.getTransmission().getLocalID() +
				"; TRANSMISSION-PART: " + msg.getPartNumber() + "/" + msg.getTotalParts() + "]";
	}
	
	/**
	 * @author mstevens
	 */
	private interface Handler
	{
		
		public void handle(int smsID, Message<?, ?> msg, int resultCode, byte[] pdu);
	}
	
	/**
	 * @author mstevens
	 */
	private class SentHandler implements Handler
	{

		@Override
		public void handle(int smsID, Message<?, ?> msg, int resultCode, byte[] pdu)
		{
			// Handle result:
			switch(resultCode)
			{
				case Activity.RESULT_OK:
					// Use call back to register send time of the message, and possibly that of the whole transmission, and save the msg/transmission to the TransmissionStore:
					msg.getTransmission().getSentCallback().onSent(msg.getPartNumber());
					Log.i(TAG, "Sending " + getMsgDescription(smsID, msg) + ": success.");
					break;
				case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
					Log.i(TAG, "Sending " + getMsgDescription(smsID, msg) + ": generic failure.");
					break;
				case SmsManager.RESULT_ERROR_NO_SERVICE:
					Log.i(TAG, "Sending " + getMsgDescription(smsID, msg) + ": no service error.");
					break;
				case SmsManager.RESULT_ERROR_NULL_PDU:
					Log.i(TAG, "Sending " + getMsgDescription(smsID, msg) + ": null PDU error.");
					break;
				case SmsManager.RESULT_ERROR_RADIO_OFF:
					Log.i(TAG, "Sending " + getMsgDescription(smsID, msg) + ": radio off error.");
					break;
				default:
					Log.e(TAG, "Unknown result code!");
			}
		}
		
	}
	
	/**
	 * @author mstevens
	 */
	private class DeliveredHandler implements Handler
	{

		@Override
		public void handle(int smsID, Message<?, ?> msg, int resultCode, byte[] pdu)
		{
			// Handle result:
			switch(resultCode)
			{
				case Activity.RESULT_OK:
					// Try to parse pdu to check delivery success & get delivery time ("discharge time"):
					TimeStamp deliveredAt = TimeStamp.now(); // use current time as fall-back
					if(pdu != null && DeviceControl.isGSM(app)) // SMSStatusReport only works on GSM/3GPP phones (not on CDMA/3GPP2)
					{
						try
						{
							SMSStatusReport statusReport = new SMSStatusReport(pdu);
							//Log.d(TAG, "SMS-STATUS-REPORT: TP-SCTS=" + TimeUtils.getISOTimestamp(statusReport.getServiceCenterTimeStamp(), true) + "; TP-DT=" + TimeUtils.getISOTimestamp(statusReport.getDischargeTime(), true));
							if(!statusReport.isReceived())
							{	 // the report indicates the message was *not* received!
								Log.i(TAG, "Delivery " + getMsgDescription(smsID, msg) + ": failed (pdu indicates non-successful).");
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
					msg.getTransmission().getSentCallback().onDelivered(msg.getPartNumber(), deliveredAt);
					Log.i(TAG, "Delivery " + getMsgDescription(smsID, msg) + ": success.");
					break;
				case Activity.RESULT_CANCELED:
					Log.i(TAG, "Delivery " + getMsgDescription(smsID, msg) + ": failure.");
					break;
			}
		}
		
	}

}
