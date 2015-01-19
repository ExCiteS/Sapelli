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

package uk.ac.ucl.excites.sapelli.collector.remote;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * 
 * @author benelliott
 */
public class Receiver
{
	static public final int RECEIVER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping RECEIVER_ID_FIELD = IntegerRangeMapping.ForSize(0, RECEIVER_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int RETRANSMIT_INTERVAL_SIZE_BITS = 32;
	private Project project;
	private Correspondent correspondent;
	private int retransmitIntervalMillis;
	private boolean encrypt;
	
	public Receiver(Project project, Correspondent correspondent, int retransmitIntervalSec, boolean encrypt)
	{
		this.project = project;
		this.correspondent = correspondent;
		this.retransmitIntervalMillis = retransmitIntervalSec;
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
	 * @param project the project to set
	 */
	public void setProject(Project project)
	{
		this.project = project;
	}

	/**
	 * @return the correspondent
	 */
	public Correspondent getCorrespondent()
	{
		return correspondent;
	}

	/**
	 * @param correspondent the correspondent to set
	 */
	public void setCorrespondent(Correspondent correspondent)
	{
		this.correspondent = correspondent;
	}

	/**
	 * @return the retransmitIntervalMillis
	 */
	public int getRetransmitIntervalMillis()
	{
		return retransmitIntervalMillis;
	}
	
	/**
	 * @param retransmitIntervalMillis the retransmitIntervalMillis to set
	 */
	public void setRetransmitIntervalMillis(int retransmitIntervalMillis)
	{
		this.retransmitIntervalMillis = retransmitIntervalMillis;
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
