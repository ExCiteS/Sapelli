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
	
	private Project project;
	private Correspondent receiver;
	
	/**
	 * Amount of seconds between transmission attempts
	 */
	private int transmitIntervalS;
	
	private boolean encrypt;
	
	public SendingSchedule(Project project, Correspondent receiver, int retransmitIntervalMillis, boolean encrypt)
	{
		this.project = project;
		this.receiver = receiver;
		this.transmitIntervalS = retransmitIntervalMillis;
		this.encrypt = encrypt;
	}

	/**
	 * @return the project
	 */
	public Project getProject()
	{
		return project;
	}

	/**
	 * @return the receiver
	 */
	public Correspondent getReceiver()
	{
		return receiver;
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
	 */
	public void setTransmitIntervalS(int transmitIntervalS)
	{
		this.transmitIntervalS = transmitIntervalS;
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
	 */
	public void setEncrypt(boolean encrypt)
	{
		this.encrypt = encrypt;
	}
	
}
