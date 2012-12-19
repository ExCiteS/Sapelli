package uk.ac.excites.transmission.snd.sms;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import uk.ac.excites.transmission.snd.R;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.telephony.SmsManager;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

/**
 * @author Michalis Vitos
 * 
 */
@SuppressLint("SimpleDateFormat")
public class SMS extends Activity
{
	private Button btnSendSMS;
	private Button btnSendBinary;
	private Spinner phoneNoSpinner;
	private EditText txtMessage;
	private TextView header;
	private TextView charCounter;
	private static final String TAG = "SMS123";
	private static final int MAX_SMS_MESSAGE_LENGTH = 160;
	private static final int SMS_PORT = 2013;

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_sms);

		// Set the spinner and request focus
		header = (TextView) findViewById(R.id.header);
		header.setFocusable(true);
		header.setFocusableInTouchMode(true);

		// Set the rest of the widgets
		phoneNoSpinner = (Spinner) findViewById(R.id.txtPhoneNoSpinner);
		btnSendSMS = (Button) findViewById(R.id.btnSendSMS);
		btnSendBinary = (Button) findViewById(R.id.btnSendBinary);
		txtMessage = (EditText) findViewById(R.id.txtMessage);
		charCounter = (TextView) findViewById(R.id.counter);

		// Set a char listener
		txtMessage.addTextChangedListener(mTextEditorWatcher);
		smsCount(txtMessage.getText());

		// On click on the button Send the SMS
		btnSendSMS.setOnClickListener(new View.OnClickListener()
		{
			public void onClick(View v)
			{
				// Get the phone number
				String phone_numbers[] = getResources().getStringArray(R.array.phones_numbers_array);
				int position = (int) phoneNoSpinner.getSelectedItemId();
				String phoneNo = phone_numbers[position];

				String message = txtMessage.getText().toString();
				if (phoneNo.length() > 0 && message.length() > 0)
					sendSMS(phoneNo, message);
				else
					Toast.makeText(getBaseContext(), "Please enter both phone number and message.", Toast.LENGTH_SHORT).show();
			}
		});

		// On click on the button Send the SMS
		btnSendBinary.setOnClickListener(new View.OnClickListener()
		{
			public void onClick(View v)
			{
				// Get the phone number
				String phone_numbers[] = getResources().getStringArray(R.array.phones_numbers_array);
				int position = (int) phoneNoSpinner.getSelectedItemId();
				String phoneNo = phone_numbers[position];

				String message = txtMessage.getText().toString();
				if (phoneNo.length() > 0 && message.length() > 0)
					sendBinary(phoneNo, message);
				else
					Toast.makeText(getBaseContext(), "Please enter both phone number and message.", Toast.LENGTH_SHORT).show();
			}
		});
	}

	/** Count the chars */
	private final TextWatcher mTextEditorWatcher = new TextWatcher()
	{
		public void beforeTextChanged(CharSequence s, int start, int count, int after)
		{

		}

		public void onTextChanged(CharSequence s, int start, int before, int count)
		{
			smsCount(s);
		}

		@Override
		public void afterTextChanged(Editable arg0)
		{

		}
	};

	private void smsCount(CharSequence s)
	{
		// This sets a textview to the current length
		String smsNo = (s.length() == 0) ? "0" : String.valueOf(s.length() / 160 + 1);
		charCounter.setText(s.length() + "/" + smsNo);
	}

	// ---sends a SMS message to another device---
	private void sendSMS(String phoneNumber, String message)
	{
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		Date date = new Date();
		Log.i(TAG, "-----------------START-------------------");
		Log.i(TAG, dateFormat.format(date).toString());

		final String SENT = "SMS_SENT";
		final String DELIVERED = "SMS_DELIVERED";

		PendingIntent sentPI = PendingIntent.getBroadcast(this, 0, new Intent(SENT), 0);

		PendingIntent deliveredPI = PendingIntent.getBroadcast(this, 0, new Intent(DELIVERED), 0);

		// ---when the SMS has been sent---
		registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context arg0, Intent arg1)
			{
				switch (getResultCode())
				{
				case Activity.RESULT_OK:
					Toast.makeText(getBaseContext(), "SMS sent", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS sent");
					break;
				case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
					Toast.makeText(getBaseContext(), "Generic failure", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Generic failure");
					break;
				case SmsManager.RESULT_ERROR_NO_SERVICE:
					Toast.makeText(getBaseContext(), "No service", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: No service");
					break;
				case SmsManager.RESULT_ERROR_NULL_PDU:
					Toast.makeText(getBaseContext(), "Null PDU", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Null PDU");
					break;
				case SmsManager.RESULT_ERROR_RADIO_OFF:
					Toast.makeText(getBaseContext(), "Radio off", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Radio off");
					break;
				}
			}
		}, new IntentFilter(SENT));

		// ---when the SMS has been delivered---
		registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context arg0, Intent arg1)
			{
				switch (getResultCode())
				{
				case Activity.RESULT_OK:
					Toast.makeText(getBaseContext(), "SMS delivered", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS delivered");
					break;
				case Activity.RESULT_CANCELED:
					Toast.makeText(getBaseContext(), "SMS not delivered", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		}, new IntentFilter(DELIVERED));

		SmsManager smsManager = SmsManager.getDefault();

		// Send multiple SMSs
		ArrayList<String> parts = smsManager.divideMessage(message);
		ArrayList<PendingIntent> sentIntents = new ArrayList<PendingIntent>();
		ArrayList<PendingIntent> deliveryIntents = new ArrayList<PendingIntent>();

		for (int i = 0; i < parts.size(); i++)
		{
			sentIntents.add(sentPI);
			deliveryIntents.add(deliveredPI);
			Log.i(TAG, "Parts Loop: " + i);
		}
		smsManager.sendMultipartTextMessage(phoneNumber, null, parts, sentIntents, deliveryIntents);

		Log.i(TAG, "Message char size: " + message.length() + " chars");
		Log.i(TAG, "There were sent " + parts.size() + " SMS");
		Log.i(TAG, "--------------------END------------------");
	}

	// ---sends a SMS message to another device---
	private void sendBinary(String phoneNumber, String message)
	{
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		Date date = new Date();
		Log.i(TAG, "-----------------START-------------------");
		Log.i(TAG, dateFormat.format(date).toString());

		final String SENT = "DATA_SENT";
		final String DELIVERED = "DATA_DELIVERED";

		PendingIntent sentPI = PendingIntent.getBroadcast(this, 0, new Intent(SENT), 0);

		PendingIntent deliveredPI = PendingIntent.getBroadcast(this, 0, new Intent(DELIVERED), 0);

		// ---when the SMS has been sent---
		registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context arg0, Intent arg1)
			{
				switch (getResultCode())
				{
				case Activity.RESULT_OK:
					Toast.makeText(getBaseContext(), "SMS sent", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS sent");
					break;
				case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
					Toast.makeText(getBaseContext(), "Generic failure", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Generic failure");
					break;
				case SmsManager.RESULT_ERROR_NO_SERVICE:
					Toast.makeText(getBaseContext(), "No service", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: No service");
					break;
				case SmsManager.RESULT_ERROR_NULL_PDU:
					Toast.makeText(getBaseContext(), "Null PDU", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Null PDU");
					break;
				case SmsManager.RESULT_ERROR_RADIO_OFF:
					Toast.makeText(getBaseContext(), "Radio off", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: Radio off");
					break;
				}
			}
		}, new IntentFilter(SENT));

		// ---when the SMS has been delivered---
		registerReceiver(new BroadcastReceiver()
		{
			@Override
			public void onReceive(Context arg0, Intent arg1)
			{
				switch (getResultCode())
				{
				case Activity.RESULT_OK:
					Toast.makeText(getBaseContext(), "SMS delivered", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS delivered");
					break;
				case Activity.RESULT_CANCELED:
					Toast.makeText(getBaseContext(), "SMS not delivered", Toast.LENGTH_SHORT).show();
					Log.i(TAG, "BroadcastReceiver: SMS not delivered");
					break;
				}
			}
		}, new IntentFilter(DELIVERED));

		SmsManager smsManager = SmsManager.getDefault();
		byte[] data = new byte[message.length()];

		for (int index = 0; index < message.length() && index < MAX_SMS_MESSAGE_LENGTH; ++index)
		{
			data[index] = (byte) message.charAt(index);
		}

		try
		{
			smsManager.sendDataMessage(phoneNumber, null, (short) SMS_PORT, data, sentPI, deliveredPI);
		} catch (Exception e)
		{
			Log.e(TAG, "Error in activity " + e.getClass().getName() + " / " + e.getMessage());
		}

		Log.i(TAG, "Message char size: " + message.length() + " chars");
		// Log.i(TAG, "There were sent " + parts.size() + " SMS");
		Log.i(TAG, "--------------------END------------------");
	}
}