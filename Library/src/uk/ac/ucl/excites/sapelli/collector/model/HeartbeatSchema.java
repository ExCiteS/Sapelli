/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model;

import uk.ac.ucl.excites.sapelli.collector.SapelliCollectorClient;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;

/**
 * 
 * @author mstevens
 */
public class HeartbeatSchema extends Schema
{
	
	static public final TimeStampColumn LOCAL_TIME_COLUMN = TimeStampColumn.Century21("LocalTime", false, false);
	static public final IntegerColumn DEVICE_ID_COLUMN = new IntegerColumn("DeviceID", true, true, Long.SIZE);
	static public final LocationColumn LAST_KNOWN_LOCATION_COLUMN = new LocationColumn("lastKnownLocation", true, true, true, false, false, true, true, true);
	//TODO static private final StringColumn NOTE_COLUMN = new StringColumn(, optional, maxLengthBytes)
	//TODO last usage timestamp?
	
	/**
	 * @param project
	 */
	public HeartbeatSchema(Project project)
	{
		super(	SapelliCollectorClient.GetModelID(project),
				(short) 0, // TODO save way to do this?
				project.getName() +
				(project.getVariant() != null ? '_' + project.getVariant() : "") +
				"_v" + project.getVersion() +
				":" + "Heartbeat");
		this.addColumn(LOCAL_TIME_COLUMN);
		this.addColumn(DEVICE_ID_COLUMN);
		this.addColumn(LAST_KNOWN_LOCATION_COLUMN);
		// TODO note
		this.seal();
	}
	
	public Record newHeartbeat(long deviceID, Location lastKnownLocation, String note)
	{
		Record heartbeat = createRecord();
		LOCAL_TIME_COLUMN.storeValue(heartbeat, TimeStamp.now());
		DEVICE_ID_COLUMN.storeValue(heartbeat, deviceID);
		LAST_KNOWN_LOCATION_COLUMN.storeValue(heartbeat, lastKnownLocation);
		// TODO note
		return heartbeat;
	}

}
