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

package uk.ac.ucl.excites.sapelli.collector.transmission;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * Class that holds information about a Record-sending relationship between a Project and some receiving Correspondent.
 * 
 * @author benelliott, mstevens
 */
public class SendSchedule
{
	
	// STATICS --------------------------------------------------------------------------
	static public final int DEFAULT_TRANSMIT_INTERVAL_SECONDS = 60 * 60; // = 1 hour
	
	static public boolean isValidForTransmission(SendSchedule schedule)
	{
		return hasValidReceiver(schedule) && schedule.isEnabled() && schedule.isIDSet();
	}
	
	static public boolean hasValidReceiver(SendSchedule schedule)
	{
		return schedule != null && schedule.getProject() != null && schedule.getReceiver() != null && !schedule.getReceiver().isUserDeleted();
	}
	
	// DYNAMICS -------------------------------------------------------------------------
	private final Project project;
	private Correspondent receiver;
	
	private Integer id;
	
	/**
	 * Amount of seconds between transmission attempts
	 */
	private int transmitIntervalS;
	
	private boolean airplaneModeCycling = false;
	
	private boolean encrypt;
	
	private boolean enabled;
	
	/**
	 * For every heartbeatInterval-th transmission attempt without data to be sent a "heartbeat" record will be sent instead.
	 * If heartbeatInterval is 0 no heartbeats are ever sent.
	 */
	private int heartbeatInterval;
	
	/**
	 * Used this to count the number of passed, subsequent transmission attempts during which there was no data to send.
	 * When this number = heartbeatInterval a heartbeat must be sent.
	 */
	private int noDataToSendCounter;
	
	public SendSchedule(Project project, Correspondent receiver, boolean enabled)
	{
		this.project = project;
		this.receiver = receiver;
		this.enabled = enabled;
		this.transmitIntervalS = DEFAULT_TRANSMIT_INTERVAL_SECONDS;
	}
	
	/**
	 * Used upon database retrieval
	 * 
	 * @param project
	 * @param id
	 * @param receiver
	 * @param transmitIntervalS
	 * @param encrypt
	 * @param enabled
	 */
	public SendSchedule(Project project, int id, Correspondent receiver, int transmitIntervalS, boolean encrypt, boolean enabled, int heartbeatInterval, int noDataToSendCounter)
	{
		this.project = project;
		this.id = id;
		this.receiver = receiver;
		this.transmitIntervalS = transmitIntervalS;
		this.encrypt = encrypt;
		this.enabled = enabled;
		this.heartbeatInterval = heartbeatInterval;
		this.noDataToSendCounter = noDataToSendCounter;
	}

	/**
	 * @return the project
	 */
	public Project getProject()
	{
		return project;
	}

	/**
	 * @return the id
	 * @throws IllegalStateException when no ID has been set
	 */
	public int getID() throws IllegalStateException
	{
		if(id == null)
			throw new IllegalStateException("ID has not been set yet");
		return id.intValue();
	}

	/**
	 * @param id the id to set
	 * @throws IllegalStateException when the Transmission already has an ID which is different from the given one
	 */
	public void setID(int id) throws IllegalStateException
	{
		if(this.id != null && this.id.intValue() != id)
			throw new IllegalStateException("A different id value has already been set (existing: " + this.id + "; new: " + id + ")!");
		this.id = id;
	}
	
	public boolean isIDSet()
	{
		return id != null;
	}

	/**
	 * @return the receiver
	 */
	public Correspondent getReceiver()
	{
		return receiver;
	}

	/**
	 * @param receiver the receiver to set
	 */
	public SendSchedule setReceiver(Correspondent receiver)
	{
		this.receiver = receiver;
		return this;
	}

	/**
	 * @return the transmitIntervalS
	 */
	public int getTransmitIntervalS()
	{
		return transmitIntervalS;
	}

	/**
	 * @param transmitIntervalS seconds between transmission attempts
	 * @return 
	 */
	public SendSchedule setTransmitIntervalS(int transmitIntervalS)
	{
		this.transmitIntervalS = transmitIntervalS;
		return this;
	}

	/**
	 * @return the airplaneModeCycling
	 */
	public boolean isAirplaneModeCycling()
	{
		return airplaneModeCycling;
	}

	/**
	 * @param airplaneModeCycling the airplaneModeCycling to set
	 */
	public void setAirplaneModeCycling(boolean airplaneModeCycling)
	{
		this.airplaneModeCycling = airplaneModeCycling;
	}

	/**
	 * @return the encrypt
	 */
	public boolean isEncrypt()
	{
		return encrypt;
	}

	/**
	 * @param encrypt the encrypt to set
	 * @return 
	 */
	public SendSchedule setEncrypt(boolean encrypt)
	{
		this.encrypt = encrypt;
		return this;
	}
	
	/**
	 * @return the enabled
	 */
	public boolean isEnabled()
	{
		return enabled;
	}

	/**
	 * @param enabled the enabled to set
	 * @return 
	 */
	public SendSchedule setEnabled(boolean enabled)
	{
		this.enabled = enabled;
		return this;
	}

	/**
	 * @return the heartbeatInterval
	 */
	public int getHeartbeatInterval()
	{
		return heartbeatInterval;
	}

	/**
	 * @param heartbeatInterval the heartbeatInterval to set
	 */
	public void setHeartbeatInterval(int heartbeatInterval)
	{
		if(this.heartbeatInterval != heartbeatInterval)
			resetNoDataToSendCounter(); // also reset counter
		this.heartbeatInterval = heartbeatInterval;
	}

	/**
	 * @return the noDataToSendCounter
	 */
	public int getNoDataToSendCounter()
	{
		return noDataToSendCounter;
	}

	public void incrementNoDataToSendCounter()
	{
		noDataToSendCounter++;
	}
	
	public void resetNoDataToSendCounter()
	{
		noDataToSendCounter = 0;
	}
	
}
