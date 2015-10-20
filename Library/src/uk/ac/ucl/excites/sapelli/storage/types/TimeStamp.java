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

package uk.ac.ucl.excites.sapelli.storage.types;

import java.io.Serializable;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormatter;

/**
 * @author mstevens
 * 
 */
public class TimeStamp implements Comparable<TimeStamp>, Serializable
{

	// STATICS-------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static private final int HOUR_MS = 60 /* minutes */* 60 /* seconds */* 1000 /* milliseconds */;
	static private final int QUARTER_OF_AN_HOUR_MS = 15 /* minutes */* 60 /* seconds */* 1000 /* milliseconds */;

	/**
	 * Returns the "raw" timezone offset in milliseconds.
	 * 
	 * <p><b>Note (2013-07-13):</b><br/>
	 * 	Implementation used to be:<br/>
	 * 		{@code return value.getZone().toTimeZone().getRawOffset();}<br/>
	 * 	But that is incorrect during summer time!<br/>
	 * 	Corrected implementation:<br/>
	 * 		{@code return value.getZone().toTimeZone().getOffset(value.getMillis());}</p>
	 * 	This could be shortened by using only Joda-time's DateTimeZone and no longer also the native TimeZone, like so:<br/>
	 * 		{@code return value.getZone().getOffset(value.getMillis());}<br/>
	 * 	However that causes weird crashes in Joda-time (probably due to a bug in the library).</p>
	 * 
	 * <p><b>Note (2015-10-14):</b><br/>
	 * 	Did new, intensive testing (with Joda-time v2.8.2) but was unable to reproduce the crashes mentioned above.
	 * 	Probably is was not a Joda-time bug after all. Changed implementation to the shorter Joda-time only one.</p>  
	 * 
	 * @param value
	 * @return
	 */
	static public int getTimeZoneOffsetMS(DateTime value)
	{
		return value.getZone().getOffset(value.getMillis());
	}

	/**
	 * Returns the "raw" timezone offset in quarters of an hour.
	 * 
	 * @param value
	 * @return
	 */
	static public int getTimeZoneOffsetQH(DateTime value)
	{
		return getTimeZoneOffsetMS(value) / QUARTER_OF_AN_HOUR_MS;
	}
	
	/**
	 * Returns the "raw" timezone offset in hours.
	 * 
	 * @param value
	 * @return
	 */
	static public float getTimeZoneOffsetH(DateTime value)
	{
		return ((float) getTimeZoneOffsetMS(value)) / HOUR_MS;
	}

	/**
	 * Returns a DateTimeZone for the given quarters of an hour offset.
	 * 
	 * @param offsetQH offset to UTC in quarters of hours
	 * @return a matching (but unnamed) DateTimeZone instance
	 */
	static public DateTimeZone getDateTimeZoneForQHOffset(int offsetQH)
	{
		return getDateTimeZoneForMSOffset(offsetQH * QUARTER_OF_AN_HOUR_MS);
	}

	/**
	 * Returns a DateTimeZone for the given millisecond offset.
	 * 
	 * <p><b>Note (2013-07-13):</b><br/>
	 * 	Implementation used to be:<br/>
	 * 		{@code return DateTimeZone.forTimeZone(uk.ac.ucl.excites.util.TimeUtils.getTimeZone(offsetMS));}<br/>
	 *	Seems to make no difference w.r.t. offset (although we do not get "named" zones this way, but the names could have been wrong anyway, due to DST).</p>
	 * 
	 * @param offsetMS offset to UTC in milliseconds
	 * @return a matching (but unnamed) DateTimeZone instance
	 */
	static public DateTimeZone getDateTimeZoneForMSOffset(int offsetMS)
	{
		return DateTimeZone.forOffsetMillis(offsetMS);
	}

	/**
	 * Current time in the current/default timezone
	 * 
	 * @return
	 */
	static public TimeStamp now()
	{
		return new TimeStamp();
	}
	
	/**
	 * Returns the latest of the 2 given time stamps.
	 * 
	 * @param t1
	 * @param t2
	 * @return
	 */
	static public TimeStamp Latest(TimeStamp t1, TimeStamp t2)
	{
		if(t1 == null)
			return t2;
		if(t2 == null)
			return t1;
		else
			return t1.msSinceEpoch >= t2.msSinceEpoch ? t1 : t2;
	}
	
	/**
	 * Returns the earliest of the 2 given time stamps.
	 * 
	 * @param t1
	 * @param t2
	 * @return
	 */
	static public TimeStamp Earliest(TimeStamp t1, TimeStamp t2)
	{
		if(t1 == null)
			return t2;
		if(t2 == null)
			return t1;
		else
			return t1.msSinceEpoch <= t2.msSinceEpoch ? t1 : t2;
	}

	// DYNAMICS------------------------------------------------------
	private final long msSinceEpoch;
	private final int quarterHourOffsetWrtUTC;
	private transient DateTime dateTime; // transient!

	/**
	 * *Now* constructor
	 */
	public TimeStamp()
	{
		this(DateTime.now());
	}
	
	/**
	 * @param msSinceEpoch
	 * @param quarterHourOffsetWrtUTC
	 */
	public TimeStamp(long msSinceEpoch, int quarterHourOffsetWrtUTC)
	{
		this.msSinceEpoch = msSinceEpoch;
		this.quarterHourOffsetWrtUTC = quarterHourOffsetWrtUTC;
	}
	
	/**
	 * Constructs a new TimeStamp with the given {@code msSinceEpoch} and the default time zone.
	 * 
	 * @param msSinceEpoch
	 */
	public TimeStamp(long msSinceEpoch)
	{
		this(new DateTime(msSinceEpoch));
	}

	/**
	 * @param dateTime
	 */
	public TimeStamp(DateTime dateTime)
	{
		this(dateTime.getMillis(), getTimeZoneOffsetQH(dateTime));
		this.dateTime = dateTime;
	}
	
	/**
	 * Constructs a new TimeStamp with the given {@code msSinceEpoch} and the given time zone.
	 * 
	 * @param msSinceEpoch
	 * @param dateTimeZone
	 */
	public TimeStamp(long msSinceEpoch, DateTimeZone dateTimeZone)
	{
		this(new DateTime(msSinceEpoch, dateTimeZone));
	}
	
	/**
	 * Copy constructor
	 * 
	 * @param timeStamp
	 */
	public TimeStamp(TimeStamp timeStamp)
	{
		this.msSinceEpoch = timeStamp.msSinceEpoch;
		this.quarterHourOffsetWrtUTC = timeStamp.quarterHourOffsetWrtUTC;
		this.dateTime = timeStamp.dateTime;
	}

	/**
	 * @return the msSinceEpoch
	 */
	public long getMsSinceEpoch()
	{
		return msSinceEpoch;
	}

	/**
	 * @return the quarterHourOffsetWrtUTC
	 */
	public int getQuarterHourOffsetWrtUTC()
	{
		return quarterHourOffsetWrtUTC;
	}
	
	/**
	 * @return the offset of the timezone wrt UTC 
	 */
	public float getHourOffsetWrtUTC()
	{
		return quarterHourOffsetWrtUTC / 4.0f;
	}

	public DateTime toDateTime()
	{
		if(dateTime == null)
			dateTime = new DateTime(msSinceEpoch, getDateTimeZoneForQHOffset(quarterHourOffsetWrtUTC));
		return dateTime;
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof TimeStamp)
		{
			TimeStamp other = (TimeStamp) obj;
			return this.msSinceEpoch == other.msSinceEpoch && this.quarterHourOffsetWrtUTC == other.quarterHourOffsetWrtUTC;
		}
		return false;
	}

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (int) (msSinceEpoch ^ (msSinceEpoch >>> Integer.SIZE));
		hash = 31 * hash + quarterHourOffsetWrtUTC;
		return hash;
	}

	@Override
	public int compareTo(TimeStamp another)
	{
		if(this == another)
			return 0;
		// Note: we cannot do (this.msSinceEpoch - other.msSinceEpoch) because that can cause overflow
		if(this.msSinceEpoch == another.msSinceEpoch)
			return 0;
		if(this.msSinceEpoch < another.msSinceEpoch)
			return -1;
		else
			return 1;
	}
	
	/**
	 * @param another - should not be null
	 * @return whether or not this TimeStamp indicates a time (strictly) before the one indicated by the given TimeStamp
	 */
	public boolean isBefore(TimeStamp another)
	{
		return this.msSinceEpoch < another.msSinceEpoch;
	}
	
	/**
	 * @param another - should not be null
	 * @return whether or not this TimeStamp indicates a time (strictly) after the one indicated by the given TimeStamp
	 */
	public boolean isAfter(TimeStamp another)
	{
		return another.msSinceEpoch < this.msSinceEpoch;
	}
	
	/**
	 * @param ms
	 * @return a new TimeStamp instance which indicates a time that differs from this TimeStamp by the given amount of milliseconds (+/-)
	 */
	public TimeStamp shift(long ms)
	{
		return new TimeStamp(this.msSinceEpoch + ms, this.quarterHourOffsetWrtUTC);
	}
	
	@Override
	public String toString()
	{
		return toDateTime().toString();
	}
	
	public String format(DateTimeFormatter formatter)
	{
		return formatter.print(toDateTime());
	}

}
