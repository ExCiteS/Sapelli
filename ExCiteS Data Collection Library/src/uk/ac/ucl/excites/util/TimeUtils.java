package uk.ac.ucl.excites.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * @author mstevens, Michalis Vitos
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
		return new SimpleDateFormat(format).format(calendar.getTime());
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
	
	static public String getTimestampForFileName(long timestamp)
	{
		return FileTimestampFormatter.print(timestamp);
	}
}
