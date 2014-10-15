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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormatter;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column to store TimeStamps.
 * Supports various ways to configure precision and (hence) size.
 * 
 * @author mstevens
 */
public class TimeStampColumn extends ComparableColumn<TimeStamp>
{
	
	// STATICS-------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static private final int TIMEZONE_QH_OFFSET_SIZE = 7; //bits
	
	static public final String LOCAL_PRETTY_VIRTUAL_COLUMN_NAME = "LocalYYYYMMDD_HHMMSS";
	static public final String UTC_OFFSET_VIRTUAL_COLUMN_NAME = "UCTOffsetH";
	static public final String RAW_TIMESTAMP_VIRTUAL_COLUMN_NAME = "UnixMS";
	
	/**
	 * Returns a TimeStampColumn that can hold Java-style timestamps, i.e. signed 64 bit integers representing
	 * the number of milliseconds since (or to, if negative) the Java/Unix epoch of 1970/01/01 00:00:00 UTC.
	 * All times are stored as UTC (local timezone is NOT kept).
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public TimeStampColumn JavaMSTime(String name, boolean optional, boolean addVirtuals)
	{
		return new TimeStampColumn(name, new TimeStamp(Long.MIN_VALUE), 64 /*bits*/, true, false, true, optional, addVirtuals);
	}
	
	/**
	 * Returns a TimeStampColumn that can hold Java-style timestamps, i.e. signed 64 bit integers representing
	 * the number of milliseconds since (or to, if negative) the Java/Unix epoch of 1970/01/01 00:00:00 UTC.
	 * Local timezone IS kept.
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public TimeStampColumn JavaMSLocalTime(String name, boolean optional, boolean addVirtuals)
	{
		return new TimeStampColumn(name, new TimeStamp(Long.MIN_VALUE), 64 /*bits*/, true, true, true, optional, addVirtuals);
	}
	
	/**
	 * Returns a TimeStampColumn that can hold any millisecond-accurate timestamp in the 21st century, including a local timezone reference.
	 * Takes up 49 bits.
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public TimeStampColumn Century21(String name, boolean optional, boolean addVirtuals)
	{
		return new TimeStampColumn(name, new TimeStamp(new DateTime(2000, 01, 01, 00, 00, 00, DateTimeZone.UTC)), new TimeStamp(new DateTime(2100, 01, 01, 00, 00, 00, DateTimeZone.UTC)), true, true, false, optional, addVirtuals);
	}
	
	/**
	 * Returns a TimeStampColumn that can hold any second-accurate timestamp in the 21st century, including a local timezone reference
	 * Takes up 39 bits.
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public TimeStampColumn Century21NoMS(String name, boolean optional, boolean addVirtuals)
	{
		return new TimeStampColumn(name, new TimeStamp(new DateTime(2000, 01, 01, 00, 00, 00, DateTimeZone.UTC)), new TimeStamp(new DateTime(2100, 01, 01, 00, 00, 00, DateTimeZone.UTC)), false, true, false, optional, addVirtuals);
	}
	
	/**
	 * Returns a TimeStampColumn that only needs 30 bits.
	 * This is achieved by using second-level accurate (instead of millisecond-level), by not storing the
	 * local timezone, and by limiting the value range to a 34 year window starting on 2008/01/01
	 * (taken because the first Android device was released in 2008).
	 * 
	 * @param name
	 * @param optional
	 * @return
	 */
	static public TimeStampColumn Compact(String name, boolean optional, boolean addVirtuals)
	{
		return new TimeStampColumn(name, new TimeStamp(new DateTime(2008, 01, 01, 00, 00, 00, DateTimeZone.UTC)), 30 /*bits*/, false, false, false, optional, addVirtuals);
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
	 * @param strictHighBound whether highBound date should be strictly respected (true) or not (false; meaning that the column will accept any TimeStamp that fits in the allocated number of bits)
	 * @param optional
	 */
	public TimeStampColumn(String name, TimeStamp lowBound, TimeStamp highBound, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		this(	name,
				new IntegerRangeMapping(	Math.round(lowBound.getMsSinceEpoch() / (keepMS ? 1 : 1000d)),
											Math.round(highBound.getMsSinceEpoch() / (keepMS ? 1 : 1000d))),
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
	public TimeStampColumn(String name, TimeStamp lowBound, int sizeBits, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		this(	name,
				IntegerRangeMapping.ForSize(	Math.round(lowBound.getMsSinceEpoch() / (keepMS ? 1 : 1000d)),
												sizeBits - (keepLocalTimezone ? TIMEZONE_QH_OFFSET_SIZE : 0)),
				keepMS,
				keepLocalTimezone,
				strictHighBound,
				optional,
				addVirtuals);
	}
	
	private TimeStampColumn(String name, IntegerRangeMapping timeMapping, boolean keepMS, boolean keepLocalTimezone, boolean strictHighBound, boolean optional, boolean addVirtuals)
	{
		super(TimeStamp.class, name, optional);
		this.timeMapping = timeMapping;
		this.keepMS = keepMS;
		this.keepLocalTimezone = keepLocalTimezone;
		this.strict = strictHighBound;
		
		if(addVirtuals)
		{
			// Add virtual version with second-accurate local(!) time in "pretty ISO" format ("yyyy-MM-dd HH:mm:ss"), which should be correctly interpreted by (most) Excel installations.
			this.addVirtualVersion(StringColumn.ForCharacterCount(LOCAL_PRETTY_VIRTUAL_COLUMN_NAME, optional, 19) , new VirtualColumn.ValueMapper<String, TimeStamp>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public String mapValue(TimeStamp nonNullValue)
				{
					return TimeUtils.PrettyTimestampWithoutMSFormatter.print(nonNullValue.toDateTime());
				}
	
				@Override
				public int hashCode()
				{
					return LOCAL_PRETTY_VIRTUAL_COLUMN_NAME.hashCode();
				}
			});
			
			// Add virtual version with offset in number of hours (signed float) of the local timezone w.r.t. UTC:
			this.addVirtualVersion(new FloatColumn(UTC_OFFSET_VIRTUAL_COLUMN_NAME, optional, true, false), new VirtualColumn.ValueMapper<Double, TimeStamp>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public Double mapValue(TimeStamp nonNullValue)
				{
					return Double.valueOf(nonNullValue.getHourOffsetWrtUTC());
				}
	
				@Override
				public int hashCode()
				{
					return UTC_OFFSET_VIRTUAL_COLUMN_NAME.hashCode();
				}
			});
			
			// Add virtual version with raw millisecond timestamp (= elapsed ms since the UNIX/Java epoch of 1970-01-01T00:00:00Z; 'Z' meaning UTC)
			this.addVirtualVersion(new IntegerColumn(RAW_TIMESTAMP_VIRTUAL_COLUMN_NAME, optional, true, Long.SIZE), new VirtualColumn.ValueMapper<Long, TimeStamp>()
			{
				private static final long serialVersionUID = 2L;
	
				@Override
				public Long mapValue(TimeStamp nonNullValue)
				{
					return nonNullValue.getMsSinceEpoch();
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
	public TimeStampColumn copy()
	{
		return new TimeStampColumn(name, timeMapping, keepMS, keepLocalTimezone, strict, optional, virtualVersions != null && virtualVersions.get(0).getName().equals(LOCAL_PRETTY_VIRTUAL_COLUMN_NAME));
	}
	
	@Override
	public TimeStamp parse(String value) throws IllegalArgumentException
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

	protected TimeStamp parse(String value, DateTimeFormatter formatter) throws IllegalArgumentException
	{
		return new TimeStamp(formatter.withOffsetParsed().parseDateTime(value)); // always parse UTC offset (even when keepLocalTimezone=false, that only affects binary storage)
	}
	
	@Override
	public String toString(TimeStamp value)
	{
		DateTime dt = value.toDateTime();
		// Note: we always keep milliseconds & UTC offset (keepMS & keepLocalTimeZone only affect binary storage)
		return TimeUtils.ISOWithMSFormatter.withZone(dt.getZone()).print(dt);
	}
	
	@Override
	protected void write(TimeStamp value, BitOutputStream bitStream) throws IOException
	{
		timeMapping.write(Math.round(value.getMsSinceEpoch() / (keepMS ? 1 : 1000d)), bitStream);
		if(keepLocalTimezone)
			bitStream.write(value.getQuarterHourOffsetWrtUTC(), TIMEZONE_QH_OFFSET_SIZE, true);
	}

	/**
	 * Note: when local time zone is not kept TimeStamps that are read from binary input will be in UTC.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#read(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream)
	 */
	@Override
	protected TimeStamp read(BitInputStream bitStream) throws IOException
	{
		long msSinceJavaEpoch = timeMapping.read(bitStream) * (keepMS ? 1 : 1000);
		return new TimeStamp(msSinceJavaEpoch, keepLocalTimezone ? TimeStamp.getDateTimeZoneFor((int) bitStream.readInteger(TIMEZONE_QH_OFFSET_SIZE, true)) : DateTimeZone.UTC);
	}		

	@Override
	protected void validate(TimeStamp value) throws IllegalArgumentException
	{
		DateTimeFormatter formatter = TimeUtils.ISOWithMSFormatter;
		if(!timeMapping.inRange(Math.round(value.getMsSinceEpoch() / (keepMS ? 1 : 1000d)), strict))
			throw new IllegalArgumentException("The given DateTime (" + value.format(formatter) + ") is outside the allowed range (from " + getLowBound().format(formatter) + " to " + getHighBound().format(formatter) + ").");
	}

	@Override
	protected int _getMinimumSize()
	{
		return timeMapping.size() + (keepLocalTimezone ? TIMEZONE_QH_OFFSET_SIZE : 0);
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return _getMinimumSize(); //size is fixed
	}
	
	public TimeStamp getLowBound()
	{
		return new TimeStamp(timeMapping.lowBound() * (keepMS ? 1 : 1000));
	}

	public TimeStamp getHighBound()
	{
		return new TimeStamp(timeMapping.highBound(strict) * (keepMS ? 1 : 1000));
	}

	@Override
	protected boolean equalRestrictions(Column<TimeStamp> otherColumn)
	{
		if(otherColumn instanceof TimeStampColumn)
		{
			TimeStampColumn other = (TimeStampColumn) otherColumn;
			return this.timeMapping.equals(other.timeMapping) && this.keepLocalTimezone == other.keepLocalTimezone && this.keepMS == other.keepMS && this.strict == other.strict;
		}
		else
			return false;
	}

	@Override
	protected TimeStamp copy(TimeStamp value)
	{
		return new TimeStamp(value);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(TimeStamp lhs, TimeStamp rhs)
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
