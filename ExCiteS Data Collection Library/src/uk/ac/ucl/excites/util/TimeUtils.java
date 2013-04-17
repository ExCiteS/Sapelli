package uk.ac.ucl.excites.util;

import java.util.Calendar;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * @author mstevens
 *
 */
public final class TimeUtils
{
	
	private static DateTimeFormatter FileTimestampFormatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HHmmss");
	
	private TimeUtils() {}

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
	
	static public TimeZone getTimeZone(int rawOffset)
	{
		String[] timeZoneIDs = TimeZone.getAvailableIDs(rawOffset);
		if(timeZoneIDs.length == 0)
			throw new IllegalArgumentException("Could not fine a timezone for offset: " + rawOffset);
		return TimeZone.getTimeZone(timeZoneIDs[0]);
	}
	
	static public String getTimestampForFileName()
	{
		return FileTimestampFormatter.print(DateTime.now());
	}
	
}
