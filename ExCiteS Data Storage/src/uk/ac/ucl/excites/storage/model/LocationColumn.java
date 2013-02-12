/**
 * 
 */
package uk.ac.ucl.excites.storage.model;

import java.io.IOException;
import java.text.ParseException;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.types.Location;

/**
 * @author mstevens
 *
 */
public class LocationColumn extends Column<Location>
{

	protected boolean doublePrecision;
	protected boolean storeAltitude;
	protected boolean storeBearing;
	protected boolean storeSpeed;
	protected boolean storeAccuracy;
	
	/**
	 * @param name
	 * @param doublePrecision
	 * @param storeAltitude
	 * @param storeBearing
	 * @param storeSpeed
	 * @param storeAccuracy
	 * @param optional
	 */
	public LocationColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeBearing, boolean storeSpeed, boolean storeAccuracy)
	{
		super(name, optional);
		this.doublePrecision = doublePrecision;
		this.storeAltitude = storeAltitude;
		this.storeBearing = storeBearing;
		this.storeSpeed = storeSpeed;
		this.storeAccuracy = storeAccuracy;
	}

	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 * @throws ParseException failed to parse at least latitude and longitude
	 * @throws NumberFormatException could not parse one of the values
	 */
	@Override
	protected Location parse(String value) throws ParseException, NumberFormatException
	{
		return Location.Parse(value);
	}

	@Override
	protected String toString(Location value)
	{
		return value.toString();
	}

	@Override
	protected void write(Location value, BitOutputStream bitStream) throws IOException
	{
		//Latitude:
		if(doublePrecision)
			bitStream.write(value.getLatitude());
		else
			bitStream.write((float) value.getLatitude());
		//Longitude:
		if(doublePrecision)
			bitStream.write(value.getLongitude());
		else
			bitStream.write((float) value.getLongitude());
		//Altitude:
		if(storeAltitude)
		{
			if(value.hasAltitude())
			{
				bitStream.write(true); //presence bit
				if(doublePrecision)
					bitStream.write(value.getAltitude());
				else
					bitStream.write((float) value.getAltitude());
			}
			else
				bitStream.write(false); //presence bit
		}
		//Bearing:
		if(storeBearing)
		{
			if(value.hasBearing())
			{
				bitStream.write(true); //presence bit
				bitStream.write(value.getBearing());
			}
			else
				bitStream.write(false); //presence bit
		}
		//Speed:
		if(storeSpeed)
		{
			if(value.hasSpeed())
			{
				bitStream.write(true); //presence bit
				bitStream.write(value.getSpeed());
			}
			else
				bitStream.write(false); //presence bit
		}
		//Accuracy:
		if(storeAccuracy)
		{
			if(value.hasAccuracy())
			{
				bitStream.write(true); //presence bit
				bitStream.write(value.getAccuracy());
			}
			else
				bitStream.write(false); //presence bit
		}
	}

	@Override
	protected Location read(BitInputStream bitStream) throws IOException
	{
		//Latitude:
		double lat = (doublePrecision ? bitStream.readDouble() : bitStream.readFloat());
		//Longitude:
		double lon = (doublePrecision ? bitStream.readDouble() : bitStream.readFloat());
		//Altitude:
		Double alt = (storeAltitude && bitStream.readBit() ? (doublePrecision ? bitStream.readDouble() : bitStream.readFloat()) : null);
		//Bearing:
		Float bea = (storeBearing && bitStream.readBit() ? bitStream.readFloat() : null);
		//Speed:
		Float spe = (storeSpeed && bitStream.readBit() ? bitStream.readFloat() : null);
		//Accuracy:
		Float acc = (storeAccuracy && bitStream.readBit() ? bitStream.readFloat() : null);
		return new Location(lat, lon, alt, bea, spe, acc);
	}

	@Override
	protected void validate(Location value) throws IllegalArgumentException
	{
		//does nothing (for now)
	}

	@Override
	public boolean isVariableSize()
	{
		return (storeAltitude || storeAccuracy || storeBearing || storeSpeed);
		/* Size is fixed if neither altitude, bearing, speed or accuracy are stored, if one or more of these
		 * fields are stored the size is variable because these fields are not always present in a Location object.
		 */
	}

	@Override
	public int getSize()
	{
		return 	(doublePrecision ? Double.SIZE : Float.SIZE) /*Lat*/ + (doublePrecision ? Double.SIZE : Float.SIZE) /*Lon*/ +
				(storeAltitude ? (1 + (doublePrecision ? Double.SIZE : Float.SIZE)) /*Alt (w/ presence bit)*/ : 0) +
				(storeBearing ? (1 + Float.SIZE) /*Bearing (w/ presence bit)*/ : 0) +
				(storeSpeed ? (1 + Float.SIZE) /*Speed (w/ presence bit)*/ : 0) +
				(storeAccuracy ? (1 + Float.SIZE) /*Acc (w/ presence bit)*/ : 0);
	}

}
