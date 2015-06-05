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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author mstevens
 *
 * @see TextSMSTransmission
 * @see TextMessage
 * @see Encoding
 */
public final class Decoding
{
	
	private Decoding(){ /*do not instantiate*/ }
	
	/**
	 * Reverse look-up table which maps 127 characters from the {@link Encoding#GSM_0338_CHAR_TABLE} alphabet back to
	 * the 7 bits unsigned integer values they represent when occurring in the body portion of the SMS content String.
	 * 
	 * @see TextSMSTransmission#unwrap()
	 * @see Encoding#GSM_0338_CHAR_TABLE
	 */
	static public final Map<Character, Integer> GSM_0338_REVERSE_CHAR_TABLE;
	static
	{
		final Map<Character, Integer> map = new HashMap<Character, Integer>(Encoding.GSM_0338_CHAR_TABLE.length - 1);
		for(int c = 0; c < Encoding.GSM_0338_CHAR_TABLE.length; c++)
			if(c != Encoding.ESCAPE_ESC)
				map.put(Encoding.GSM_0338_CHAR_TABLE[c], c);
		GSM_0338_REVERSE_CHAR_TABLE = Collections.unmodifiableMap(map);
	}
	
	/**
	 * Reverse look-up table which maps 64 characters from the {@link Encoding#GSM_0338_CHAR_TABLE} alphabet back to
	 * the 6 bits unsigned integer values they represent when occurring in the header portion of the SMS content String.
	 * 
	 * @see TextMessage#TextMessage(uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent, String, uk.ac.ucl.excites.sapelli.storage.types.TimeStamp)
	 * @see Encoding#GSM_0338_CHAR_TABLE
	 * @see Encoding#HEADER_CHAR_INDEX_MAPPING
	 */
	static public final Map<Character, Integer> HEADER_VALUE_REVERSE_MAPPING;
	static
	{
		final Map<Character, Integer> map = new HashMap<Character, Integer>(Encoding.HEADER_CHAR_INDEX_MAPPING.length);
		for(int i = 0; i < Encoding.HEADER_CHAR_INDEX_MAPPING.length; i++)
			map.put(Encoding.GSM_0338_CHAR_TABLE[Encoding.HEADER_CHAR_INDEX_MAPPING[i]], i);
		HEADER_VALUE_REVERSE_MAPPING = Collections.unmodifiableMap(map);
	}
	
}
