/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormatter;

import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column to store DateTime objects (cfr: http://joda-time.sourceforge.net)
 * Supports various ways to configure precision and (hence) size
 * 
 * @author mstevens
 */
public class DateTimeColumn extends ComparatorColumn<DateTime>
{
	
	// STATICS-------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static private final int HOUR_MS = 60 /*minutes*/ * 60 /*seconds*/ * 1000 /*milliseconds*/;
	static private final int QUARTER_OF_AN_HOUR_MS = 15 /*minutes*/ * 60 /*seconds*/ * 1000 /*milliseconds*/;
	static private final int TIMEZONE_QH_OFFSET_SIZE = 7; //bits
	
	static public final String LOCAL_PRETTY_VIRTUAL_COLUMN_NAME = "LocalYYYYMMDD_HHMMSS";
	static public final String UTC_OFFSET_VIRTUAL_COLUMN_NAME = "UCTOffsetH";
	static public final String RAW_TIMESTAMP_VIRTUAL_COLUMN_NAME = "UnixMS";
	
	/**
	 * Returns a DateTimeColumn that can hold Java-style timestamps, i.e. signed 64 bit integers representing
	 * the number of milliseconds since (or to, if negative) the Java/Unix epoch of 1970/01/01 00:00:00 UTC.
	 * All times are stored as UTC (local timezone is NOT kept).
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public DateTimeColumn JavaMSTime(String name, boolean optional, boolean addVirtuals)
	{
		return new DateTimeColumn(name, new DateTime(Long.MIN_VALUE), 64 /*bits*/, true, false, true, optional, addVirtuals);
		/* //Alternative:
		 *  return new DateTimeColumn(name, new DateTime(Long.MIN_VALUE), new DateTime(Long.MAX_VALUE), true, false, true, optional);
		 */
	}
	
	/**
	 * Returns a DateTimeColumn that can hold any millisecond-accurate timestamp in the 21st century, including a local timezone reference.
	 * Takes up 49 bits.
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public DateTimeColumn Century21(String name, boolean optional, boolean addVirtuals)
	{
		return new DateTimeColumn(name, new DateTime(2000, 01, 01, 00, 00, 00, DateTimeZone.UTC), new DateTime(2100, 01, 01, 00, 00, 00, DateTimeZone.UTC), true, true, false, optional, addVirtuals);
	}
	
	/**
	 * Returns a DateTimeColumn that can hold any second-accurate timestamp in the 21st century, including a local timezone reference
	 * Takes up 39 bits.
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public DateTimeColumn Century21NoMS(String name, boolean optional, boolean addVirtuals)
	{
		return new DateTimeColumn(name, new DateTime(2000, 01, 01, 00, 00, 00, DateTimeZone.UTC), new DateTime(2100, 01, 01, 00, 00, 00, DateTimeZone.UTC), false, true, false, optional, addVirtuals);
	}
	
	/**
	 * Returns a DateTimeColumn that only needs 30 bits.
	 * This is achieved by using second-level accurate (instead of millisecond-level), by not storing the
	 * local timezone, and by limiting the value range to a 34 year window starting on 2008/01/01
	 * (taken because the first Android device was released in 2008).
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public DateTimeColumn Compact(String name, boolean optional, boolean addVirtuals)
	{
		return new DateTimeColumn(name, new DateTime(2008, 01, 01, 00, 00, 00, DateTimeZone.UTC), 30 /*bits*/, false, false, false, optional, addVirtuals);
	}
	
	/**
	 * Note (2013-07-13):
	 * 	Implementation used to be:
	 * 		return value.getZone().toTimeZone().getRawOffset();
	 *  But that is incorrect during summer time!
	 *  The current implementation could be shortened by using only Joda-time's DateTimeZone and no longer also the native TimeZone, like so
	 * 		return value.getZone().getOffset(value.getMillis());
	 * 	However that causes weird crashes in Joda-time (probably due to a bug in the library).
	 * 
	 * @param value
	 * @return
	 */
	static public int getTimeZoneOffsetMS(DateTime value)
	{
		return value.getZone().toTimeZone().getOffset(value.getMillis());
	}

	static public int getTimeZoneOffsetQH(DateTime value)
	{
		return getTimeZoneOffsetMS(value) / QUARTER_OF_AN_HOUR_MS;
	}
	
	/**
	 * Note (2013-07-13):
	 * 	Implementation used to be: return DateTimeZone.forTimeZone(uk.ac.ucl.excites.util.TimeUtils.getTimeZone(quarterHourOffset * QUARTER_OF_AN_HOUR_MS));
	 * Seems to make no difference w.r.t. offset (although we do not get "named" zones this way, but the names could have been wrong anyway, due to DST)
	 * 
	 * @param quarterHourOffset
	 * @return
	 */
	static public DateTimeZone getDateTimeZoneFor(int quarterHourOffset)
	{
		return DateTimeZone.forOffsetMillis(quarterHourOffset * QUARTER_OF_AN_HOUR_MS);
	}
	
	static public float getTimeZoneOffsetH(DateTime value)
	{
		return ((float) getTimeZoneOffsetMS(value)) / HOUR_MS; 
	}
	
	// DYNAMICS------------------------------------------------------
	protected IntegerRangeMapping timeMapping;
	protected boolean keepMS;
	protected boolean keepLocalTimezone;
	protected boolean strict;

	/**
	 * @param name
	 * @param lowBound earliest allowed DateTime (inclusive)
	 * @param highBound latest allowed DateTime (exclusive)
	 * @param keepMS whether to use millisecond-level (true) or second-level (false) accuracy
	 * @param keepLocalTimezone whether or not to remember to local timezone
	 * @param strictHighBound whether highBound date should be strictly respected (true) or not (false; meaning that the column will accept any DateTime that fits in the allocated number of bits)
	 * @param optional
	 */
	public DateTimeColumn(String name, DateTime lowBound, DateTime highBound, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		this(	name,
				new IntegerRangeMapping(	Math.round(lowBound.getMillis() / (keepMS ? 1 : 1000d)),
											Math.round(highBound.getMillis() / (keepMS ? 1 : 1000d))),
				keepMS,
				keepLocalTimezone,
				strictHighBound,
				optional,
				addVirtuals);
	}
	
	/**
	 * @param name
	 * @param lowBound earliest allowed DateTime (inclusive)
	 * @param the number of bits to use
	 * @param keepMS whether to use millisecond-level (true) or second-level (false) accuracy
	 * @param keepLocalTimezone whether or not to remember to local timezone
	 * @param strictHighBound whether highBound date should be strictly respected (true) or not (false; meaning that the column will accept any DateTime that fits in the allocated number of bits)
	 * @param optional
	 */
	public DateTimeColumn(String name, DateTime lowBound, int sizeBits, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		this(	name,
				IntegerRangeMapping.ForSize(	Math.round(lowBound.getMillis() / (keepMS ? 1 : 1000d)),
												sizeBits - (keepLocalTimezone ? TIMEZONE_QH_OFFSET_SIZE : 0)),
				keepMS,
				keepLocalTimezone,
				strictHighBound,
				optional,
				addVirtuals);
	}
	
	private DateTimeColumn(String name, IntegerRangeMapping timeMapping, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		super(DateTime.class, name, optional);
		this.timeMapping = timeMapping;
		this.keepMS = keepMS;
		this.keepLocalTimezone = keepLocalTimezone;
		this.strict = strictHighBound;
		
		if(addVirtuals)
		{
			// Add virtual version with second-accurate local(!) time in "pretty ISO" format ("yyyy-MM-dd HH:mm:ss"), which should be correctly interpreted by (most) Excel installations.
			this.addVirtualVersion(StringColumn.ForCharacterCount(LOCAL_PRETTY_VIRTUAL_COLUMN_NAME, optional, 19) , new VirtualColumn.ValueMapper<String, DateTime>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public String mapValue(DateTime nonNullValue)
				{
					return TimeUtils.PrettyTimestampWithoutMSFormatter.print(nonNullValue);
				}
	
				@Override
				public int hashCode()
				{
					return LOCAL_PRETTY_VIRTUAL_COLUMN_NAME.hashCode();
				}
			});
			
			// Add virtual version with offset in number of hours (signed float) of the local timezone w.r.t. UTC:
			this.addVirtualVersion(new FloatColumn(UTC_OFFSET_VIRTUAL_COLUMN_NAME, optional, true, false), new VirtualColumn.ValueMapper<Double, DateTime>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public Double mapValue(DateTime nonNullValue)
				{
					return Double.valueOf(getTimeZoneOffsetH(nonNullValue));
				}
	
				@Override
				public int hashCode()
				{
					return UTC_OFFSET_VIRTUAL_COLUMN_NAME.hashCode();
				}
			});
			
			// Add virtual version with raw millisecond timestamp (= elapsed ms since the UNIX/Java epoch of 1970-01-01T00:00:00Z; 'Z' meaning UTC)
			this.addVirtualVersion(new IntegerColumn(RAW_TIMESTAMP_VIRTUAL_COLUMN_NAME, optional, true, Long.SIZE), new VirtualColumn.ValueMapper<Long, DateTime>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public Long mapValue(DateTime nonNullValue)
				{
					return nonNullValue.getMillis();
				}
	
				@Override
				public int hashCode()
				{
					return RAW_TIMESTAMP_VIRTUAL_COLUMN_NAME.hashCode();
				}
			});
		}
	}
	
	@Override
	public DateTimeColumn copy()
	{
		return new DateTimeColumn(name, timeMapping, keepMS, keepLocalTimezone, strict, optional, virtualVersions != null && virtualVersions.get(0).getName().equals(LOCAL_PRETTY_VIRTUAL_COLUMN_NAME));
	}
	
	@Override
	public DateTime parse(String value) throws IllegalArgumentException
	{
		try
		{
			return parse(value, TimeUtils.ISOWithMSFormatter);
		}
		catch(IllegalArgumentException iae)
		{
			return parse(value, TimeUtils.ISOWithoutMSFormatter); //for compatibility with old XML exports which used ISO without milliseconds when keepMS=false
		}
	}

	protected DateTime parse(String value, DateTimeFormatter formatter) throws IllegalArgumentException
	{
		return formatter.withOffsetParsed().parseDateTime(value); // always parse UTC offset (even when keepLocalTimezone=false, that only affects binary storage)
	}
	
	@Override
	public String toString(DateTime value)
	{
		// Note: we always keep milliseconds & UTC offset (keepMS & keepLocalTimeZone only affect binary storage)
		return TimeUtils.ISOWithMSFormatter.withZone(value.getZone()).print(value);
	}
	
	@Override
	protected void write(DateTime value, BitOutputStream bitStream) throws IOException
	{
		timeMapping.write(Math.round(value.getMillis() / (keepMS ? 1 : 1000d)), bitStream);
		if(keepLocalTimezone)
			bitStream.write(getTimeZoneOffsetQH(value), TIMEZONE_QH_OFFSET_SIZE, true);
	}

	@Override
	protected DateTime read(BitInputStream bitStream) throws IOException
	{
		long msSinceJavaEpoch = timeMapping.read(bitStream) * (keepMS ? 1 : 1000);
		return new DateTime(msSinceJavaEpoch, keepLocalTimezone ? getDateTimeZoneFor((int) bitStream.readInteger(TIMEZONE_QH_OFFSET_SIZE, true)) : null);
	}		

	@Override
	protected void validate(DateTime value) throws IllegalArgumentException
	{
		DateTimeFormatter formatter = keepMS ? TimeUtils.ISOWithMSFormatter : TimeUtils.ISOWithoutMSFormatter;
		if(!timeMapping.inRange(Math.round(value.getMillis() / (keepMS ? 1 : 1000d)), strict))
			throw new IllegalArgumentException("The given DateTime (" + formatter.print(value) + ") is outside the allowed range (from " + formatter.print(getLowBound()) + " to " + formatter.print(getHighBound()) + ").");
	}

	@Override
	protected int _getMinimumSize()
	{
		return timeMapping.getSize() + (keepLocalTimezone ? TIMEZONE_QH_OFFSET_SIZE : 0);
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return _getMinimumSize(); //size is fixed
	}
	
	public DateTime getLowBound()
	{
		return new DateTime(timeMapping.getLowBound() * (keepMS ? 1 : 1000));
	}

	public DateTime getHighBound()
	{
		return new DateTime(timeMapping.getHighBound(strict) * (keepMS ? 1 : 1000));
	}

	@Override
	protected boolean equalRestrictions(Column<DateTime> otherColumn)
	{
		if(otherColumn instanceof DateTimeColumn)
		{
			DateTimeColumn other = (DateTimeColumn) otherColumn;
			return this.timeMapping.equals(other.timeMapping) && this.keepLocalTimezone == other.keepLocalTimezone && this.keepMS == other.keepMS && this.strict == other.strict;
		}
		else
			return false;
	}

	@Override
	protected DateTime copy(DateTime value)
	{
		return new DateTime(value);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(DateTime lhs, DateTime rhs)
	{
		return lhs.compareTo(rhs);
	}

	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + (timeMapping == null ? 0 : timeMapping.hashCode());
		hash = 31 * hash + (keepMS ? 0 : 1);
		hash = 31 * hash + (keepLocalTimezone ? 0 : 1);
		hash = 31 * hash + (strict ? 0 : 1);
		return hash;
	}
	
}
