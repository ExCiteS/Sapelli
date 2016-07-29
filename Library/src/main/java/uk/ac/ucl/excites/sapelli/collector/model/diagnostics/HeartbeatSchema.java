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

package uk.ac.ucl.excites.sapelli.collector.model.diagnostics;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.types.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;

/**
 * The schema for heartbeat records providing diagnostic information about the device and the usage of a specific project
 * 
 * @author mstevens
 */
public class HeartbeatSchema extends Schema
{
	
	static private final long serialVersionUID = 2L;
	
	static public final String HEARTBEAT_NAME = "Heartbeat";
	
	static public final TimeStampColumn	COLUMN_LOCAL_TIME = TimeStampColumn.Century21("LocalTime", false, true);
	static public final IntegerColumn	COLUMN_DEVICE_ID = new IntegerColumn("DeviceID", false, true, Long.SIZE);
	static public final IntegerColumn	COLUMN_UPTIME_MS = new IntegerColumn("UptimeMS", true, false, 40); // 40 bits: allowing for uptimes of almost 35 years ;-)
	static public final IntegerColumn	COLUMN_BATTERY_LEVEL = new IntegerColumn("BatteryLevel", true, 0, 100);
	static public final LocationColumn	COLUMN_LAST_KNOWN_LOCATION = new LocationColumn("lastKnownLocation", true, true, true, false, false, true, true, true);
	static public final TimeStampColumn	COLUMN_PROJECT_LAST_OPENED_AT = TimeStampColumn.Century21("ProjectLastOpenedAt", false, true);
	
	/**
	 * HeartbeatSchema constructor
	 * 
	 * @param project the project in the context of which the heartbeats will be produced
	 * @throws ModelFullException
	 */
	public HeartbeatSchema(Project project)
	{
		// Call Schema constructor (the schema will be added to the project model): 
		super(	project.getModel(),
				project.getModel().getName() + ":" + HEARTBEAT_NAME,
				CollectorClient.GetSchemaTableName(
					null,
					CollectorClient.SCHEMA_FLAGS_COLLECTOR_AUX_DATA,
					Project.class.getSimpleName() + project.getID() + "_" + project.getFingerPrint() + "_" + HEARTBEAT_NAME,
					null),
				CollectorClient.SCHEMA_FLAGS_COLLECTOR_AUX_DATA);
		// Add columns:
		this.addColumn(COLUMN_LOCAL_TIME);
		this.addColumn(COLUMN_DEVICE_ID);
		this.addColumn(COLUMN_UPTIME_MS);
		this.addColumn(COLUMN_BATTERY_LEVEL);
		this.addColumn(COLUMN_LAST_KNOWN_LOCATION);
		this.addColumn(COLUMN_PROJECT_LAST_OPENED_AT);
		// Add primary key on LocalTime & DeviceID:
		this.setPrimaryKey(PrimaryKey.WithColumnNames(COLUMN_LOCAL_TIME, COLUMN_DEVICE_ID));
		// Seal the schema:
		this.seal();
	}
	
	/**
	 * Method to create new heartbeats (i.e. Records with this HeartbeatSchema as their schema). All arguments are optional (i.e. can be null).
	 * 
	 * @param deviceID
	 * @param uptimeMS
	 * @param batteryLevelPercentage
	 * @param lastKnownLocation
	 * @param projectLastOpenedAt
	 * @return
	 */
	public Record newHeartbeat(Long deviceID, Long uptimeMS, Float batteryLevelPercentage, Location lastKnownLocation, TimeStamp projectLastOpenedAt)
	{
		Record heartbeat = createRecord();
		COLUMN_LOCAL_TIME.storeValue(heartbeat, TimeStamp.now());
		COLUMN_DEVICE_ID.storeValue(heartbeat, deviceID);
		COLUMN_UPTIME_MS.storeValue(heartbeat, uptimeMS);
		if(batteryLevelPercentage != null)
			COLUMN_BATTERY_LEVEL.storeValue(heartbeat, Math.round(batteryLevelPercentage));
		COLUMN_LAST_KNOWN_LOCATION.storeValue(heartbeat, lastKnownLocation);
		COLUMN_PROJECT_LAST_OPENED_AT.storeValue(heartbeat, projectLastOpenedAt);
		return heartbeat;
	}

}
