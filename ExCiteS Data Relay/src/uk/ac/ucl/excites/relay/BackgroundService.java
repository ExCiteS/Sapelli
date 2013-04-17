package uk.ac.ucl.excites.relay;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;

import uk.ac.ucl.excites.relay.sms.SmsDatabaseSQLite;
import uk.ac.ucl.excites.relay.sms.SmsObject;
import uk.ac.ucl.excites.relay.util.BinaryHelpers;
import uk.ac.ucl.excites.relay.util.Debug;
import uk.ac.ucl.excites.relay.util.Hashing;
import uk.ac.ucl.excites.relay.util.Utilities;
import android.app.AlarmManager;
import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.IBinder;
import android.telephony.SmsMessage;
import android.util.Base64;
import android.webkit.URLUtil;

/**
 * A service that checks if the phone received any SMS messages and POST them to the server
 * 
 * @author Michalis Vitos
 * 
 */
public class BackgroundService extends Service
{

	// Define some variables
	private Context mContext;
	private boolean mAllowRebind; // indicates whether onRebind should be used
	private static BroadcastReceiver smsReceiver;
	private static SmsDatabaseSQLite dao;
	private static String SERVER_URL;
	private static int TIME_SCHEDULE;
	private static ScheduledExecutorService scheduleTaskExecutor;
	private static ScheduledFuture<?> mScheduledFuture;
	private static boolean isSending;

	AlarmManager mAlarmManager;

	@Override
	public void onCreate()
	{
		// The service is being created
		this.mContext = this;
		// Initialise the database
		dao = new SmsDatabaseSQLite(mContext);

		// Add an SMS receiver
		registerSmsReceiver();

		// Creates a thread pool that can schedule commands to run after a given
		// delay, or to execute periodically.
		scheduleTaskExecutor = Executors.newScheduledThreadPool(1);

		isSending = false;

		// TODO test
		// dao.populateDb(10);
		// dao.retrieveSmsObjects();

		// Wait for the Debugger to be attached
		// android.os.Debug.waitForDebugger();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		// Get the server URL and the time Schedule
		SERVER_URL = Preferences.getServerAddress(mContext);
		TIME_SCHEDULE = Preferences.getTimeSchedule(mContext);

		setServiceForeground(mContext);

		// Check if the scheduleTaskExecutor is running and stop it first
		if(isSending)
		{
			mScheduledFuture.cancel(true);
			isSending = false;
		}

		// Creates and executes a periodic action that becomes enabled first
		// after the given initial delay, and subsequently with the given period
		mScheduledFuture = scheduleTaskExecutor.scheduleAtFixedRate(new SendingTask(), 0, TIME_SCHEDULE, TimeUnit.SECONDS);

		return START_STICKY;
	}

	/**
	 * Class to manage the sending of SMSes
	 * 
	 * @author Michalis Vitos
	 * 
	 */
	private class SendingTask implements Runnable
	{
		@Override
		public void run()
		{
			try
			{
				// Try to send all the Sms Objects
				isSending = true;
				sendSmsObjects();
			}
			catch(Exception e)
			{
				Debug.e(e);
				Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
			}
		}
	}

	/**
	 * Method to retrieve and try to post all the Sms Objects of the db
	 */
	private void sendSmsObjects()
	{
		List<SmsObject> smsList = dao.getUnsentSms();

		String response = null;
		for(SmsObject sms : smsList)
		{
			// Check if there is connectivity
			if(Utilities.isOnline(mContext))
			{
				try
				{
					response = null;
					response = postSmsObject(sms);
					if(response != null)
						Debug.d("POST sms: " + sms.getId() + " and the response is: " + (response.length() > 60 ? response.substring(0, 50) : response));
					else
						Debug.d("An error has occured upon sending the SMS.");
				}
				catch(Exception e)
				{
					Debug.e(e);
				}
			}
			else
			{
				Debug.d("No Internet Connection");
			}

			// Check if response is null
			if(response != null && response.substring(0, 3).equalsIgnoreCase("OK:"))
			{
				String[] responseStrings = response.split(":");
				long idPart = Long.valueOf(responseStrings[1]);

				// Check if the post was successful and delete the SMS from the db
				if(idPart == sms.getId())
				{
					// Update the sms table
					dao.updateSent(sms);
				}
			}
		}
	}

	/**
	 * Method to POST a SmsObject to the Server
	 * 
	 */
	private String postSmsObject(SmsObject smsObject)
	{
		HttpClient httpClient = new DefaultHttpClient();
		// Check the validity of the URL
		HttpPost httpPost = null;
		if(SERVER_URL == null)
		{
			Debug.d("URL is null!");
			return null;
		}
		Debug.d("URL: " + SERVER_URL);
		if(URLUtil.isValidUrl(SERVER_URL))
		{
			httpPost = new HttpPost(SERVER_URL);
			// Log.i(Constants.TAG, "--!-- SERVER_URL: " + SERVER_URL);
		}
		else
		{
			Debug.d("--!-- SERVER_URL ERROR --!--");
			return null;
		}

		// Set the POST parameters
		List<NameValuePair> nameValuePairList = new ArrayList<NameValuePair>();
		nameValuePairList.add(new BasicNameValuePair("smsID", String.valueOf(smsObject.getId())));
		nameValuePairList.add(new BasicNameValuePair("smsPhoneNumber", smsObject.getTelephoneNumber()));
		nameValuePairList.add(new BasicNameValuePair("smsTimestamp", String.valueOf(smsObject.getMessageTimestamp())));

		String data = smsObject.getMessageData();
		nameValuePairList.add(new BasicNameValuePair("smsData", data));

		// POST them
		try
		{
			// UrlEncodedFormEntity is an entity composed of a list of
			// url-encoded pairs.
			// This is typically useful while sending an HTTP POST request.
			UrlEncodedFormEntity urlEncodedFormEntity = new UrlEncodedFormEntity(nameValuePairList);

			// setEntity() hands the entity (here it is urlEncodedFormEntity) to
			// the request.
			httpPost.setEntity(urlEncodedFormEntity);

			try
			{
				// HttpResponse is an interface just like HttpPost.
				// Therefore we can't initialise them
				HttpResponse httpResponse = httpClient.execute(httpPost);

				// According to the JAVA API, InputStream constructor do
				// nothing.
				// So we can't initialise InputStream although it is not an
				// interface
				InputStream inputStream = httpResponse.getEntity().getContent();
				InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
				BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
				StringBuilder stringBuilder = new StringBuilder();
				String bufferedStrChunk = null;
				while((bufferedStrChunk = bufferedReader.readLine()) != null)
				{
					stringBuilder.append(bufferedStrChunk);
				}
				return stringBuilder.toString();

			}
			catch(ClientProtocolException e)
			{
				Debug.e(e);

			}
			catch(IOException e)
			{
				Debug.e(e);
			}

		}
		catch(UnsupportedEncodingException e)
		{
			Debug.e(e);
		}

		return null;
	}

	/**
	 * Register an SMS Data (Binary) Receiver
	 */
	private void registerSmsReceiver()
	{
		smsReceiver = new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context context, Intent intent)
			{
				Debug.i("Received Binary SMS");

				Bundle bundle = intent.getExtras();
				SmsMessage[] msgs = null;

				if(null != bundle)
				{
					// In telecommunications the term (PDU) means protocol data
					// unit.
					// There are two ways of sending and receiving SMS messages:
					// by text mode and by PDU (protocol description unit) mode.
					// The PDU string contains not only the message, but also a
					// lot of meta-information about the sender, his SMS service
					// center, the time stamp etc
					// It is all in the form of hexa-decimal octets or decimal
					// semi-octets.
					Object[] pdus = (Object[]) bundle.get("pdus");
					msgs = new SmsMessage[pdus.length];

					for(int i = 0; i < msgs.length; i++)
					{
						// Create the Message
						msgs[i] = SmsMessage.createFromPdu((byte[]) pdus[i]);
						// Get Message Parameters
						SmsObject receivedSms = new SmsObject();
						receivedSms.setTelephoneNumber(msgs[i].getOriginatingAddress());
						receivedSms.setMessageTimestamp(msgs[i].getTimestampMillis());
						receivedSms.setMessageData(Base64.encodeToString(msgs[i].getUserData(), Base64.CRLF));
						
						Debug.d("Received SMS and it's content hash is: "
								+ BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(msgs[i].getUserData()).toByteArray()));

						// Store the SmsObject to the db
						dao.storeSms(receivedSms);
					}
				}

				// This will stop the Broadcast and not allow the message to
				// be interpreted by the default Android app or other apps
				abortBroadcast();
			}
		};

		// Set up the Receiver Parameters
		IntentFilter mIntentFilter = new IntentFilter();
		mIntentFilter.setPriority(999);
		mIntentFilter.addAction("android.intent.action.DATA_SMS_RECEIVED");
		mIntentFilter.addDataScheme("sms");
		// Set the Port that is listening to
		mIntentFilter.addDataAuthority("*", "2013");
		// mIntentFilter.addDataType(type)
		registerReceiver(smsReceiver, mIntentFilter);
		Debug.d("Set up BinarySMS receiver.");
	}

	@SuppressWarnings("deprecation")
	public void setServiceForeground(Context mContext)
	{
		final int myID = 9999;

		// The intent to launch when the user clicks the expanded notification
		Intent mIntent = new Intent(mContext, BackgroundActivity.class);
		mIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
		PendingIntent pendIntent = PendingIntent.getActivity(mContext, 0, mIntent, 0);

		// This constructor is deprecated. Use Notification.Builder instead
		Notification mNotification = new Notification(R.drawable.ic_launcher, getString(R.string.title_activity_main), System.currentTimeMillis());

		// This method is deprecated. Use Notification.Builder instead.
		mNotification.setLatestEventInfo(this, getString(R.string.title_activity_main), getString(R.string.notification), pendIntent);

		mNotification.flags |= Notification.FLAG_NO_CLEAR;
		startForeground(myID, mNotification);
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public boolean onUnbind(Intent intent)
	{
		// All clients have unbound with unbindService()
		return mAllowRebind;
	}

	@Override
	public void onRebind(Intent intent)
	{
		// A client is binding to the service with bindService(),
		// after onUnbind() has already been called
	}

	@Override
	public void onDestroy()
	{
		try
		{
			// The service is no longer used and is being destroyed but before
			// is destroyed:
			// unregister the SMS receiver
			unregisterReceiver(smsReceiver);
			// Close the db4o database
			dao.close();

			// Close the Scheduled
			mScheduledFuture.cancel(true);
			stopSelf();
			int pid = android.os.Process.myPid();
			Debug.d("BackgroundService: onDestroy() + killProcess(" + pid + ") ");
			android.os.Process.killProcess(pid);

		}
		catch(Exception e)
		{
			Debug.e(e);
		}
	}
}