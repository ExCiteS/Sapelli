package uk.ac.ucl.excites.sapelli.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * BroadcastReceiver which listens for incoming SMS messages, and pass each to a new instance of SMSReceiverService. 
 * 
 * Note that BroadcastReceivers run on the main thread and Android punishes them for doing too much work (in fact it kills them after 10 seconds)
 * so work is moved to the asynchronous service as soon as possible.
 * 
 * Much of the behaviour of this class is the same regardless of whether the message it receives is from a binary/data SMS or a text SMS. Thus an abstract isBinary()
 * method is included and subclasses will listen for different intents (i.e. only data SMS or only text SMS).
 * 
 * @author benelliott
 *
 */
public abstract class SMSBroadcastReceiver extends BroadcastReceiver
{
	// private static final String TAG = "SMSBroadcastReceiver";
	
	/**
	 * Called when the BroadcastReceiver hears a broadcast it is interested in (i.e. received SMS).
	 * Will start the SMSReceiverService, passing the message(s) using an Intent.
	 */
	@Override
	public void onReceive(Context context, Intent intent)
	{
		// get PDU byte arrays from received SMS intent:
		byte[][] pdus = (byte[][])intent.getExtras().get("pdus");
		for (byte[] pdu : pdus)
		{
			// for each message, create a new intent to launch the SMSReceiverService (effectively queues the messages):
			Intent launchReceiverService = new Intent(context, SMSReceiverService.class);
			// attach the PDU to the intent:
			launchReceiverService.putExtra(SMSReceiverService.PDU_BYTES_EXTRA_NAME, pdu);
			// also include whether or not the PDU represents a binary/data SMS:
			launchReceiverService.putExtra(SMSReceiverService.BINARY_FLAG_EXTRA_NAME, this.isBinary()); // subclass will decide whether or not message is binary since they are registered to different intents
			// launch the service using the intent:
			context.startService(launchReceiverService);
		}
		
		/**
		 * On pre-KitKat devices (API 19) calling abortBroadcast on an SMS message will prevent lower-priority receivers from "hearing" the broadcast.
		 * On API 19+ devices (and reportedly also API 18) this call will be completely ignored by the OS because the SMS APIs have been reworked and one can only "consume"
		 * SMS broadcasts by registering as the default SMS app (which we don't really want to do, as it would mean worrying about ALL SMSs).
		 */
		abortBroadcast();
	}
	
	/**
	 * To be overriden by subclasses that listen for different Intents (either Data SMS or Textual SMS Intents).
	 * @return whether or not the SMS PDU passed by this BroadcastReceiver represents a binary (data) SMS as opposed to a textual one.
	 */
	public abstract boolean isBinary();
}
