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

package uk.ac.ucl.excites.sapelli.util;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import android.telephony.PhoneNumberUtils;

/**
 * A helper class to parse (from pdu byte[]) and represent a GSM SMS-STATUS-REPORT message (= delivery report).
 * 
 * Only works for GSM/3GPP networks (not CDMA/3GPP2).
 * 
 * Based on the source code of the following Android classes:
 * 	- com.android.internal.telephony.gsm.SmsMessage (almost everything)
 *  - com.android.internal.telephony.uicc.IccUtils (1 method)
 * All licensed under the Apache License, Version 2.0.
 * The code is taken from Android v5.1.0_r1 (+ 1 line from v4.2_r1).
 * 
 * @author mstevens
 */
public class SMSStatusReport
{
    static final String LOG_TAG = SMSStatusReport.class.getSimpleName();

    /**
     * TP-Message-Type-Indicator
     * 9.2.3
     */
    private int mMti;

    /**
     *  TP-Status - status of a previously submitted SMS.
     *  This field applies to SMS-STATUS-REPORT messages.  0 indicates success;
     *  see TS 23.040, 9.2.3.15 for description of other possible values.
     */
    private int mStatus;

    /**
     *  TP-Status - status of a previously submitted SMS.
     *  This field is true iff the message is a SMS-STATUS-REPORT message.
     */
    private boolean mIsStatusReportMessage = false;

    /**
     * TP-Service-Centre-Time-Stamp
     */
    private DateTime serviceCenterTimeStamp;
    
    /**
     * TP-Discharge-Time
     * 
     * @see http://en.wikipedia.org/wiki/GSM_03.40#Discharge_Time
     */
    private DateTime dischargeTime;
    
    /**
     * Constructor
     * 
     * @param pdu
     */
    public SMSStatusReport(byte[] pdu)
    {
    	// Parse:
    	createFromPdu(pdu);
    	
    	if(!mIsStatusReportMessage)
    		throw new IllegalArgumentException("This is not the pdu of a GSM SMS-STATUS-REPORT message");
    }
    
    /**
     * TS 27.005 3.1, &lt;pdu&gt; definition "In the case of SMS: 3GPP TS 24.011 [6]
     * SC address followed by 3GPP TS 23.040 [3] TPDU in hexadecimal format:
     * ME/TA converts each octet of TP data unit into two IRA character long
     * hex number (e.g. octet with integer value 42 is presented to TE as two
     * characters 2A (IRA 50 and 65))" ...in the case of cell broadcast,
     * something else...
     * 
     * @param pdu
     * 
     * @see Adapted from {@link com.android.internal.telephony.gsm.SmsMessage#createFromPdu(byte[])} (originally static)
     */
    private void createFromPdu(byte[] pdu)
    {
        PduParser p = new PduParser(pdu);

        /*Object mScAddress = */p.getSCAddress();

        // TP-Message-Type-Indicator
        // 9.2.3
        int firstByte = p.getByte();
        
        mMti = firstByte & 0x3;
        switch (mMti)
        {
	        // TP-Message-Type-Indicator
	        // 9.2.3
	        case 0:
	        case 3: //GSM 03.40 9.2.3.1: MTI == 3 is Reserved.
	                //This should be processed in the same way as MTI == 0 (Deliver)
	            //parseSmsDeliver(p, firstByte);
	            break;
	        case 1:
	            //parseSmsSubmit(p, firstByte);
	            break;
	        case 2:
	            parseSmsStatusReport(p, firstByte);
	            break;
	        default:
	            throw new RuntimeException("Unsupported message type");
        }
    }
    
    /**
     * Parses a SMS-STATUS-REPORT message.
     *
     * @param p A PduParser, cued past the first byte.
     * @param firstByte The first byte of the PDU, which contains MTI, etc.
     */
    private void parseSmsStatusReport(PduParser p, int firstByte)
    {
		mIsStatusReportMessage = true;

		// TP-Message-Reference
		/*int mMessageRef = */p.getByte();
		// TP-Recipient-Address
		/*Object mRecipientAddress = */p.getAddress();
		// TP-Service-Centre-Time-Stamp
		serviceCenterTimeStamp = p.getSCTimestampDateTime();
		// TP-Discharge-Time (line taken from Android v4.2_r1)
		dischargeTime = p.getSCTimestampDateTime();
		// TP-Status
		mStatus = p.getByte();

        // The following are optional fields that may or may not be present.
        if (p.moreDataPresent())
        {/*
            // TP-Parameter-Indicator
            int extraParams = p.getByte();
            int moreExtraParams = extraParams;
            while ((moreExtraParams & 0x80) != 0) {
                // We only know how to parse a few extra parameters, all
                // indicated in the first TP-PI octet, so skip over any
                // additional TP-PI octets.
                moreExtraParams = p.getByte();
            }
            // As per 3GPP 23.040 section 9.2.3.27 TP-Parameter-Indicator,
            // only process the byte if the reserved bits (bits3 to 6) are zero.
            if ((extraParams & 0x78) == 0) {
                // TP-Protocol-Identifier
                if ((extraParams & 0x01) != 0) {
                    mProtocolIdentifier = p.getByte();
                }
                // TP-Data-Coding-Scheme
                if ((extraParams & 0x02) != 0) {
                    mDataCodingScheme = p.getByte();
                }
                // TP-User-Data-Length (implies existence of TP-User-Data)
                if ((extraParams & 0x04) != 0) {
                    boolean hasUserDataHeader = (firstByte & 0x40) == 0x40;
                    parseUserData(p, hasUserDataHeader);
                }
            }*/
        }
    }
    
    /**
     * @return whether or not the original message was received on the receiver handset
     */
    public boolean isReceived()
    {
        return mStatus == 0;
    }
    
    /**
	 * @return the serviceCenterTimeStamp
	 */
	public DateTime getServiceCenterTimeStamp()
	{
		return serviceCenterTimeStamp;
	}

	/**
	 * @return the dischargeTime
	 */
	public DateTime getDischargeTime()
	{
		return dischargeTime;
	}

	private static class PduParser
    {
    	static private final int QUARTER_OF_AN_HOUR_MS = 15 /* minutes */* 60 /* seconds */* 1000 /* milliseconds */;    	
    	
		byte mPdu[];
		int mCur;

		PduParser(byte[] pdu)
		{
			mPdu = pdu;
			mCur = 0;
		}

		/**
		 * Parse and return the SC address prepended to SMS messages coming via the TS 27.005 / AT interface.
		 * Returns null on invalid address
		 */
		String getSCAddress()
		{
			int len;
			String ret;

			// length of SC Address
			len = getByte();

			if(len == 0)
			{
				// no SC address
				ret = null;
			}
			else
			{
				// SC address
				try
				{
					ret = PhoneNumberUtils.calledPartyBCDToString(mPdu, mCur, len);
				}
				catch(RuntimeException tr)
				{
					ret = null;
				}
			}
			mCur += len;
			return ret;
		}

        /**
         * returns non-sign-extended byte value
         */
        int getByte()
        {
            return mPdu[mCur++] & 0xff;
        }

        /**
         * Any address except the SC address (eg, originating address)
         * See TS 23.040 9.1.2.5
         * 
         * mstevens: Made NON-FUNCTIONAL to remove dependency on internal Android classes. Always returns null but skips right number of bytes.
         */
        Object/*GsmSmsAddress*/ getAddress()
        {
            //GsmSmsAddress ret;

            // "The Address-Length field is an integer representation of
            // the number field, i.e. excludes any semi-octet containing only
            // fill bits."
            // The TOA field is not included as part of this
            int addressLength = mPdu[mCur] & 0xff;
            int lengthBytes = 2 + (addressLength + 1) / 2;

            /*try {
                ret = new GsmSmsAddress(mPdu, mCur, lengthBytes);
            } catch (ParseException e) {
                ret = null;
                //This is caught by createFromPdu(byte[] pdu)
                throw new RuntimeException(e.getMessage());
            }*/

            mCur += lengthBytes;
            return null;//ret;
        }

		/**
		 * Parses an SC timestamp and returns a DateTime with original timezone information
		 * 
		 * @see Adapted from {@link com.android.internal.telephony.gsm.SmsMessage.PduParser#getSCTimestampMillis()}
		 * @see http://en.wikipedia.org/wiki/GSM_03.40#Time_Format
		 */
		DateTime getSCTimestampDateTime()
		{
			// TP-Service-Centre-Time-Stamp
			int year = gsmBcdByteToInt(mPdu[mCur++]);
			int month = gsmBcdByteToInt(mPdu[mCur++]);
			int day = gsmBcdByteToInt(mPdu[mCur++]);
			int hour = gsmBcdByteToInt(mPdu[mCur++]);
			int minute = gsmBcdByteToInt(mPdu[mCur++]);
			int second = gsmBcdByteToInt(mPdu[mCur++]);

			// For the timezone, the most significant bit of the least significant nibble is the sign byte
			// (meaning the max range of this field is 79 quarter-hours, which is more than enough)

			byte tzByte = mPdu[mCur++];

			// Mask out sign bit.
			int timezoneOffset = gsmBcdByteToInt((byte) (tzByte & (~0x08)));
			timezoneOffset = ((tzByte & 0x08) == 0) ? timezoneOffset : -timezoneOffset; 

			return new DateTime(year >= 90 ? year + 1900 : year + 2000, // only last 2 digits of year are in pda
								month,
								day,
								hour,
								minute,
								second,
								DateTimeZone.forOffsetMillis(timezoneOffset * QUARTER_OF_AN_HOUR_MS)); // Timezone offset is in quarter hours
		}
        
        /**
         * Decodes a GSM-style BCD byte, returning an int ranging from 0-99.
         *
         * In GSM land, the least significant BCD digit is stored in the most
         * significant nibble.
         *
         * Out-of-range digits are treated as 0 for the sake of the time stamp,
         * because of this:
         *
         * TS 23.040 section 9.2.3.11
         * "if the MS receives a non-integer value in the SCTS, it shall
         * assume the digit is set to 0 but shall store the entire field
         * exactly as received"
         * 
         * @see Taken from com.android.internal.telephony.uicc.IccUtils
         */
        public static int gsmBcdByteToInt(byte b)
        {
            int ret = 0;

            // treat out-of-range BCD values as 0
            if((b & 0xf0) <= 0x90)
                ret = (b >> 4) & 0xf;
            if((b & 0x0f) <= 0x09)
                ret +=  (b & 0xf) * 10;
            return ret;
        }

        public boolean moreDataPresent()
        {
            return (mPdu.length > mCur);
        }
        
    }

}
