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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.in;

import org.joda.time.DateTime;

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Handler;
import android.os.Looper;
import android.telephony.SmsMessage;
import android.util.Log;
import android.widget.Toast;
import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.transmission.SignalMonitoringService;
import uk.ac.ucl.excites.sapelli.collector.transmission.control.AndroidTransmissionController;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.InvalidMessageException;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;

/**
 * IntentService which handles the reception of SMS messages (passed to it from SMSBroadcastReceiver)
 * and the sending of resend requests for incomplete transmissions.
 * 
 * @author benelliott, mstevens
 */
public class IncomingSMSReceiverService extends SignalMonitoringService
{
	
	// STATIC -------------------------------------------------------
	private static final String TAG = IncomingSMSReceiverService.class.getSimpleName();

	public static final int TASK_RECEIVE_MESSAGE = 0;
	public static final int TASK_REQUEST_RESEND = 1;
	public static final int TASK_SCHEDULE_RESEND_REQUESTS = 2;
	
	/**
	 * Intent extra key for specifying task
	 */
	public static final String EXTRA_TASK = "task";
	
	/**
	 * Intent extra key for passing the PDU bytes,
	 * used when receiving a message (task = {@link #TASK_RECEIVE_MESSAGE})
	 */
	public static final String EXTRA_PDU_BYTES = "pdu";
	
	/**
	 * Intent extra key for passing whether or not the message is binary (data message),
	 * used when receiving a message (task = {@link #TASK_RECEIVE_MESSAGE})
	 */
	public static final String EXTRA_BINARY_FLAG = "binary";
	
	/**
	 * Intent extra key for passing local ID of the SMSTransmission to request a resend for,
	 * used when receiving a message (task = {@link #TASK_REQUEST_RESEND})
	 */
	public static final String EXTRA_TRANSMISSION_ID = "localID";
	
	/**
	 * If resend request cannot be sent due to lack of network service we will
	 * wait 6 minutes before trying again. 
	 */
	private static final int RESEND_REQUEST_RETRY_DELAY_MS = 6 * 60 * 1000; 
	
	/**
	 * Starts the SMSReceiverService to receive an SMS message *now*.
	 * 
	 * @param context
	 * @param pdu
	 * @param binaryMsg
	 */
	public static void ReceiveMessage(Context context, byte[] pdu, boolean binaryMsg)
	{
		Intent serviceIntent = new Intent(context, IncomingSMSReceiverService.class);
		// set task for service:
		serviceIntent.putExtra(EXTRA_TASK, TASK_RECEIVE_MESSAGE);
		// attach the PDU to the intent:
		serviceIntent.putExtra(EXTRA_PDU_BYTES, pdu);
		// also include whether or not the PDU represents a binary/data SMS:
		serviceIntent.putExtra(EXTRA_BINARY_FLAG, binaryMsg);
		// launch the service using the intent:
		context.startService(serviceIntent);
	}
	
	/**
	 * @param context
	 * @param localID local ID of an incomplete SMSTransmission
	 * @param time at which to send the request, or null (in which case no request will be scheduled and existing will be cancelled)
	 */
	public static void ScheduleResendRequest(Context context, int localID, DateTime time)
	{
		SetOrCancelResendRequestAlarm(context, localID, time);
	}
	
	/**
	 * @param context
	 * @param localID local ID of an SMSTransmission
	 */
	public static void CancelResendRequest(Context context, int localID)
	{
		SetOrCancelResendRequestAlarm(context, localID, null /*cancel*/);
	}
	
	/**
	 * @param context
	 * @param localID
	 * @param alarmTime the DateTime at which the alarm goes off, or {@code null} if an existing request must be cancelled
	 */
	private static void SetOrCancelResendRequestAlarm(Context context, int localID, DateTime alarmTime)
	{
		Log.i(TAG, "Setting/cancelling resend request alerm for transmission with local ID: " + localID);
		
		AlarmManager am = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
		
		Intent serviceIntent = new Intent(context, IncomingSMSReceiverService.class);
		serviceIntent.putExtra(EXTRA_TASK, TASK_REQUEST_RESEND);
		serviceIntent.putExtra(EXTRA_TRANSMISSION_ID, localID);
		PendingIntent pi = PendingIntent.getService(context, localID, serviceIntent, 0);
	
		// Cancel any previously scheduled alarm for the same transmission (i.e. if cancel=false we will effectively postpone the running of a previously set alarm)
		am.cancel(pi); // (tested & found working, only cancels alarm for specified localID)
		
		// Set alarm, unless we are only cancelling (indicated by null alarmTime):
		if(alarmTime != null)
		{
			am.set(AlarmManager.RTC, alarmTime.getMillis(), pi);
			// make sure alarm is scheduled again after device reboot:
			SetupBootReceiver(context, true);
		}
	}
	
	/**
	 * Starts the SMSReceiverService to schedule resend requests.
	 * Called from BootListener.
	 * 
	 * @param context
	 */
	public static void ScheduleAllResendRequests(Context context)
	{
		Intent serviceIntent = new Intent(context, IncomingSMSReceiverService.class);
		// set task for service:
		serviceIntent.putExtra(EXTRA_TASK, TASK_SCHEDULE_RESEND_REQUESTS);
		// launch the service using the intent:
		context.startService(serviceIntent);
	}
	
	/**
	 * @param enabled
	 */
	private static void SetupBootReceiver(Context context, boolean enabled)
	{
		ComponentName receiver = new ComponentName(context, BootListener.class);
		int newState = (enabled) ? PackageManager.COMPONENT_ENABLED_STATE_ENABLED : PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		context.getPackageManager().setComponentEnabledSetting(receiver, newState, PackageManager.DONT_KILL_APP);
		Log.d(TAG, (enabled ? "En" : "Dis") + "abled " + BootListener.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private CollectorApp app;
	private AndroidTransmissionController transmissionController;
	private Handler mainHandler; // only used in order to display Toasts

	public IncomingSMSReceiverService()
	{
		super(TAG);
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		mainHandler = new Handler(Looper.getMainLooper());
		try
		{
			// Use application context or SMS callbacks will be invalidated when this service terminates:
			app = (CollectorApp) getApplication();
			transmissionController = new AndroidTransmissionController(app);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Exception upon creation of in transmission controller.", e);
		}
	}

	/**
	 * Called when the service is started through an intent, which should also contain SMS messages to be read by the service.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		if(!intent.hasExtra(EXTRA_TASK))
			Log.e(TAG, "No service task specified!");
		// Decide which task to execute:
		switch(intent.getIntExtra(EXTRA_TASK, -1))
		{
			case TASK_RECEIVE_MESSAGE : receiveMessage(intent); break;
			case TASK_REQUEST_RESEND : requestResend(intent); break;
			case TASK_SCHEDULE_RESEND_REQUESTS : scheduleAllResendRequests(); break;
		}
	}
	
	private void receiveMessage(Intent intent)
	{		
		if(!intent.hasExtra(EXTRA_PDU_BYTES) || !intent.hasExtra(EXTRA_BINARY_FLAG))
		{	// should never happen
			Log.e(TAG, "Missing extras!");
			return;
		}
		
		// get PDU from intent:
		byte[] pdu = intent.getByteArrayExtra(EXTRA_PDU_BYTES);
		boolean binary = intent.getBooleanExtra(EXTRA_BINARY_FLAG, false);
		
		Log.d(TAG, (binary ? "Binary" : "Textual") + " SMS received by Sapelli " + getClass().getSimpleName());
		
		// Treat incoming message:
		try
		{
			// Create a (Sapelli) Message object from the PDU:
			final Message<?, ?> message = messageFromPDU(pdu, binary);

			// Receive/decode:
			transmissionController.receiveSMS(message);
			
			// Show toast when in debug mode:
			if(BuildConfig.DEBUG)
				mainHandler.post(new Runnable()
				{
					@Override
					public void run()
					{
						Toast.makeText(IncomingSMSReceiverService.this, "Sapelli SMS received from phone number " + message.getSender().getPhoneNumberInternational(), Toast.LENGTH_SHORT).show();
					}
				});
		}
		catch(InvalidMessageException e)
		{
			Log.i(TAG, "Received SMS message was found not to be relevant to Sapelli.");
			Log.d(TAG, "\t" + InvalidMessageException.class.getSimpleName(), e);
		}
		catch(Exception e)
		{
			Log.e(TAG, "An error occurred while trying to parse the received SMS.", e);
		}
	}
	
	/**
	 * Creates and returns a Message object from the provided PDU if possible
	 * @param pdu the message PDU (protocol data unit - header and payload) as a byte array
	 * @return a BinaryMessage or TextMessage object depending on the type of the message provided
	 * @throws InvalidMessageException if the message was definitely not relevant to Sapelli
	 * @throws Exception for any other errors that occur while trying to parse the message
	 */
	private Message<?, ?> messageFromPDU(byte[] pdu, boolean binary) throws InvalidMessageException, Exception
	{
		// Get Android SMS msg representation for pdu:
		SmsMessage androidMsg = SmsMessage.createFromPdu(pdu);
		if(androidMsg == null)
			throw new Exception("Android could not parse the SMS message from its PDU.");
		
		// Debug:
		//Log.d(TAG,"MESSAGE BODY: " + androidMsg.getMessageBody());
		
		// Get sender phone number (we assume that if the number doesn't start with '+' the correct country is given by the current network (rather than the SIM)):
		PhoneNumber senderPhoneNumber = SMSCorrespondent.toPhoneNumber(androidMsg.getOriginatingAddress(), DeviceControl.getNetworkCountryISOCode(this));
		// Get an SMSCorrespondent corresponding to this phone number:
		SMSCorrespondent sender = transmissionController.getSendingCorrespondentFor(senderPhoneNumber, binary);
		
		// Return Sapelli Message:
		if(binary)
			return new BinaryMessage(sender, androidMsg.getUserData());
		else
			return new TextMessage(sender, androidMsg.getMessageBody());
	}
	
	private void requestResend(Intent intent)
	{
		if(!intent.hasExtra(EXTRA_TRANSMISSION_ID))
		{	// should never happen
			Log.e(TAG, "Missing extras!");
			return;
		}
		
		int localID = intent.getIntExtra(EXTRA_TRANSMISSION_ID, -1);
		Log.d(TAG, "Woken by alarm for sending resend request for transmission with local ID: " + localID);
		try
		{
			if(signalMonitor.isInService())
			{	// Send out resend request for any incomplete SMS transmissions:
				transmissionController.sendSMSResendRequest(localID);
			}
			else
			{	// Try again in 6 minutes:
				ScheduleResendRequest(getApplicationContext(), localID, TimeStamp.now().shift(RESEND_REQUEST_RETRY_DELAY_MS).toDateTime());
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon trying to send resend request(s).", e);
		}
	}
	
	private void scheduleAllResendRequests()
	{
		try
		{
			boolean requestsScheduled = transmissionController.scheduleSMSResendRequests();
			SetupBootReceiver(app, requestsScheduled); // dis/enable bootlistener based on outcome (if no requests were scheduled bootlistener doesn't need to run next time, unless it is re-enabled)
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon trying to schedule resend requests.", e);
		}
	}

	/* (non-Javadoc)
	 * @see android.app.IntentService#onDestroy()
	 */
	@Override
	public void onDestroy()
	{
		if(transmissionController != null)
			transmissionController.discard();
	}
	
	/**
	 * BroadcastReceiver that listens for device boot events and when one is received, schedules resend requests for any incomplete SMSTransmissions.
	 */
	static public class BootListener extends BroadcastReceiver
	{

		@Override
		public void onReceive(Context context, Intent intent)
		{
			Log.d(getClass().getSimpleName(), "Boot event received, starting " + IncomingSMSReceiverService.class.getSimpleName() + " to schedule resend requests...");
			ScheduleAllResendRequests(context);
		}

	}

}
