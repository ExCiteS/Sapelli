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

package uk.ac.ucl.excites.sapelli.transmission.model;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;

/**
 * TODO rename
 * 
 * @author benelliott
 */
public class Receiver
{
	static public final int RECEIVER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping RECEIVER_ID_FIELD = IntegerRangeMapping.ForSize(0, RECEIVER_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int RETRANSMIT_INTERVAL_SIZE_BITS = 32;
	private int projectId;
	private int correspondentName;
	private int retransmitIntervalSec;
	private boolean encrypt;
	
	public Receiver(int projectId, int correspondentName, int retransmitIntervalSec, boolean encrypt)
	{
		this.projectId = projectId;
		this.correspondentName = correspondentName;
		this.retransmitIntervalSec = retransmitIntervalSec;
		this.encrypt = encrypt;
	}

	/**
	 * @return the projectId
	 */
	public int getProjectId()
	{
		return projectId;
	}

	/**
	 * @param projectId the projectId to set
	 */
	public void setProjectId(int projectId)
	{
		this.projectId = projectId;
	}

	/**
	 * @return the correspondentName
	 */
	public int getCorrespondentName()
	{
		return correspondentName;
	}

	/**
	 * @param correspondentName the correspondentName to set
	 */
	public void setCorrespondentName(int correspondentName)
	{
		this.correspondentName = correspondentName;
	}

	/**
	 * @return the retransmitIntervalSec
	 */
	public int getRetransmitIntervalSec()
	{
		return retransmitIntervalSec;
	}

	/**
	 * @param retransmitIntervalSec the retransmitIntervalSec to set
	 */
	public void setRetransmitIntervalSec(int retransmitIntervalSec)
	{
		this.retransmitIntervalSec = retransmitIntervalSec;
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
