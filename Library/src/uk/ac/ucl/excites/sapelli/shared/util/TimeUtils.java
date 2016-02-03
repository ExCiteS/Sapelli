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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public class TimeUtils
{

	static public final int SEC_IN_MIN = 60;
	public static final int ONE_HOUR_MS = 60 /* minutes */* SEC_IN_MIN /* seconds */* 1000 /* milliseconds */;
	public static final int QUARTER_OF_AN_HOUR_MS = 15 /* minutes */* 60 /* seconds */* 1000 /* milliseconds */;
	
	public static final DateTimeFormatter ISOWithMSFormatter = ISODateTimeFormat.dateTime();
	public static final DateTimeFormatter ISOWithoutMSFormatter = ISODateTimeFormat.dateTimeNoMillis();
	public static final DateTimeFormatter FileTimestampFormatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH.mm.ss");
	public static final DateTimeFormatter PrettyTimestampWithoutMSFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm:ss");
	public static final DateTimeFormatter PrettyTimestampWithMSFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm:ss.SSS");

	protected TimeUtils() throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot instantiate TimeUtils object");
	}

	static public Calendar getCalendar(int year, int month, int day, TimeZone timeZone)
	{
		Calendar date = Calendar.getInstance();
		date.clear();
		date.setTimeZone(timeZone);
		date.set(year, month, day);
		return date;
	}

	static public Calendar getCalendar(long msSinceJavaEpoch, TimeZone timeZone)
	{
		Calendar date = Calendar.getInstance();
		date.clear();
		date.setTimeZone(timeZone);
		date.setTimeInMillis(msSinceJavaEpoch);
		return date;
	}

	static public Calendar getCurrentCalendar()
	{
		return Calendar.getInstance();
	}

	/**
	 * Return a Calendar object that is shifted (either to the future or the past) from the current time by a given amount of time units.
	 * E.g.: getShiftedCalendar(Calendar.MINUTE, 5) returns the (local) time 5 minutes into the future.
	 *
	 * @param calendarField expected to be a valid Calendar field index (http://docs.oracle.com/javase/6/docs/api/constant-values.html#java.util.Calendar.MINUTE)
	 * @param amount to add (or subtract, if negative)
	 * @return the resulting calendar
	 */
	static public Calendar getShiftedCalendar(int calendarField, int amount)
	{
		Calendar cal = getCurrentCalendar();
		cal.add(calendarField, amount);
		return cal;
	}

	static public String formatTime(Calendar calendar, String format)
	{
		return new SimpleDateFormat(format, Locale.getDefault()).format(calendar.getTime());
	}

	static public TimeZone getTimeZone(int rawOffset)
	{
		String[] timeZoneIDs = TimeZone.getAvailableIDs(rawOffset); //TODO does this really expect raw offsets (as in, no daylight savings)?
		if(timeZoneIDs.length == 0)
			throw new IllegalArgumentException("Could not fine a timezone for offset: " + rawOffset);
		return TimeZone.getTimeZone(timeZoneIDs[0]);
	}
	
	static public String getISOTimestamp(boolean withMS)
	{
		if(withMS)
			return ISOWithMSFormatter.print(DateTime.now());
		else
			return ISOWithoutMSFormatter.print(DateTime.now());
	}

	static public String getISOTimestamp(DateTime dateTime, boolean withMS)
	{
		if(withMS)
			return ISOWithMSFormatter.print(dateTime);
		else
			return ISOWithoutMSFormatter.print(dateTime);
	}
	
	static public String getISOTimestamp(long timestamp, boolean withMS)
	{
		if(withMS)
			return ISOWithMSFormatter.print(timestamp);
		else
			return ISOWithoutMSFormatter.print(timestamp);
	}

	static public String getTimestampForFileName()
	{
		return FileTimestampFormatter.print(DateTime.now());
	}

	static public String getTimestampForFileName(DateTime dateTime)
	{
		return FileTimestampFormatter.print(dateTime);
	}
	
	static public String getTimestampForFileName(long timestamp)
	{
		return FileTimestampFormatter.print(timestamp);
	}

	static public String getPrettyTimestamp()
	{
		return PrettyTimestampWithoutMSFormatter.print(DateTime.now());
	}
	
	static public String getPrettyTimestamp(DateTime dateTime)
	{
		return PrettyTimestampWithoutMSFormatter.print(dateTime);
	}
	
	static public String getPrettyTimestamp(long timestamp)
	{
		return PrettyTimestampWithoutMSFormatter.print(timestamp);
	}
	
}
