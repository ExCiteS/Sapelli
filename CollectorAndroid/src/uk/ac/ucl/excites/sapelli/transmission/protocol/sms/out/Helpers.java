/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.transmission.protocol.sms.out;

import android.content.Intent;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;

/**
 * @author Michalis Vitos, mstevens
 */
/*package*/ final class Helpers
{
	
	static public final String EXTRA_SMS_ID = "SMS_ID";
	static public final String EXTRA_MULTI_PART_NUMBER = "MULTI_PART_NUMBER";
	static public final String EXTRA_MULTI_PART_TOTAL = "MULTI_PART_TOTAL";
	
	static public final String EXTRA_TRANSMISSION_LOCAL_ID = "TRANSMISSION_LOCAL_ID";
	static public final String EXTRA_TRANSMISSION_PART_NUMBER = "TRANSMISSION_PART_NUMBER";
	static public final String EXTRA_TRANSMISSION_NUMBER_OF_PARTS = "TRANSMISSION_NUMBER_OF_PART";
	static public final String EXTRA_TRANSMISSION_BINARY = "TRANSMISSION_BINARY_SMS";

	static public final String EXTRA_CALLBACK_ACTION_ID = "CALLBACK_ACTION_ID";
	
	static public final int[] ACTIONS_IDS = { R.string.action_sms_sent, R.string.action_sms_delivered };
	
	/**
	 * Intent extra key for the callback result code
	 */
	public static final String EXTRA_RESULT_CODE = "resultCode";
	
	static public final String EXTRA_PDU = "pdu";
	
	/**
	 * @author mstevens
	 */
	static public class SMSInfo
	{
		
		static SMSInfo FromIntent(Intent intent)
		{
			return new SMSInfo(	intent.getExtras().getInt(EXTRA_SMS_ID),
								intent.getExtras().getInt(EXTRA_MULTI_PART_NUMBER),
								intent.getExtras().getInt(EXTRA_MULTI_PART_TOTAL),
								intent.getExtras().getInt(EXTRA_TRANSMISSION_LOCAL_ID), 
								intent.getExtras().getInt(EXTRA_TRANSMISSION_PART_NUMBER),
								intent.getExtras().getInt(EXTRA_TRANSMISSION_NUMBER_OF_PARTS),
								intent.getExtras().getBoolean(EXTRA_TRANSMISSION_BINARY));
		}
		
		/**
		 * Android-level identification of the SMS being sent.
		 */
		public final int smsID;
		/**
		 * Android-level message-splitting part number, a value from [1, multiPartTotal].
		 */
		public final int multiPartNumber;
		/**
		 * Android-level message-splitting total parts.
		 */
		public final int multiPartTotal;
		
		/**
		 * local ID of a {@link SMSTransmission} of which a part is being sent.
		 */
		public final int transmissionLocalID;
		
		/**
		 * partNumber of the {@link Message} being sent.
		 */
		public final int transmissionPartNumber;
		
		/**
		 * total number of parts the {@link SMSTransmission}.
		 */
		public final int transmissionNumberOfParts;
		
		/**
		 * Whether the transmission is a {@link BinarySMSTransmission} or a {@link TextSMSTransmission}. 
		 */
		public final boolean transmissionBinary;
		
		/**
		 * @param message
		 * @param smsID
		 * @param multiPartNumber a value from [1, multiPartTotal]
		 * @param multiPartTotal
		 */
		public SMSInfo(Message<?, ?> message, int smsID, int multiPartNumber, int multiPartTotal)
		{
			this(	smsID,
					multiPartNumber,
					multiPartTotal,
					message.getTransmission().getLocalID(),
					message.getPartNumber(),
					message.getTotalParts(),
					(message instanceof BinaryMessage));
		}		


		public SMSInfo(int smsID, int multiPartNumber, int multiPartTotal, int transmissionLocalID, int transmissionPartNumber, int transmissionNumberOfParts, boolean transmissionBinary)
		{
			this.smsID = smsID;
			this.multiPartNumber = multiPartNumber;
			this.multiPartTotal = multiPartTotal;
			this.transmissionLocalID = transmissionLocalID;
			this.transmissionPartNumber = transmissionPartNumber;
			this.transmissionNumberOfParts = transmissionNumberOfParts;
			this.transmissionBinary = transmissionBinary;
		}

		public void setIntentExtras(Intent intent)
		{
			intent.putExtra(EXTRA_SMS_ID, smsID);
			intent.putExtra(EXTRA_MULTI_PART_NUMBER, multiPartNumber);
			intent.putExtra(EXTRA_MULTI_PART_TOTAL, multiPartTotal);
			intent.putExtra(EXTRA_TRANSMISSION_LOCAL_ID, transmissionLocalID);
			intent.putExtra(EXTRA_TRANSMISSION_PART_NUMBER, transmissionPartNumber);
			intent.putExtra(EXTRA_TRANSMISSION_NUMBER_OF_PARTS, transmissionNumberOfParts);
			intent.putExtra(EXTRA_TRANSMISSION_BINARY, transmissionBinary);
		}
		
	}
	
	private Helpers() {}

}
