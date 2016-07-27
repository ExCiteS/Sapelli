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

package uk.ac.ucl.excites.sapelli.storage.util;

import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public final class TimeStampUtils extends TimeUtils
{

	private TimeStampUtils() throws UnsupportedOperationException
	{
		super(); // throws UnsupportedOperationException
	}
	
	static public String getISOTimestamp(TimeStamp timeStamp, boolean withMS)
	{
		return getISOTimestamp(timeStamp.toDateTime(), withMS);
	}
	
	static public String getTimestampForFileName(TimeStamp timeStamp)
	{
		return FileTimestampFormatter.print(timeStamp.toDateTime());
	}
	
	static public String getPrettyTimestamp(TimeStamp timeStamp)
	{
		return PrettyTimestampWithoutMSFormatter.print(timeStamp.toDateTime());
	}
	
}
