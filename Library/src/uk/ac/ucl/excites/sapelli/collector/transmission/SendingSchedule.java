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
public class SendingSchedule
{
	
	private final Project project;
	private Correspondent receiver;
	
	private Integer id;
	
	/**
	 * Amount of seconds between transmission attempts
	 */
	private int transmitIntervalS;
	
	private boolean encrypt;
	
	private boolean enabled;
	
	public SendingSchedule(Project project, boolean enabled)
	{
		this.project = project;
		this.enabled = enabled;
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
	public SendingSchedule(Project project, int id, Correspondent receiver, int transmitIntervalS, boolean encrypt, boolean enabled)
	{
		this.project = project;
		this.id = id;
		this.receiver = receiver;
		this.transmitIntervalS = transmitIntervalS;
		this.encrypt = encrypt;
		this.enabled = enabled;
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
	 */
	public int getID()
	{
		if(id == null)
			throw new IllegalStateException("ID has not been set yet");
		return id.intValue();
	}

	/**
	 * @param id the id to set
	 */
	public void setID(int id)
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
	public SendingSchedule setReceiver(Correspondent receiver)
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
	public SendingSchedule setTransmitIntervalS(int transmitIntervalS)
	{
		this.transmitIntervalS = transmitIntervalS;
		return this;
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
	public SendingSchedule setEncrypt(boolean encrypt)
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
	public SendingSchedule setEnabled(boolean enabled)
	{
		this.enabled = enabled;
		return this;
	}
	
}
