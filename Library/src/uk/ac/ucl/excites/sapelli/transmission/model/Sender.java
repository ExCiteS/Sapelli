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
public class Sender
{
	static public final int SENDER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping SENDER_ID_FIELD = IntegerRangeMapping.ForSize(0, SENDER_ID_SIZE); // unsigned(!) 24 bit integer
	
	private int projectId;
	private String correspondentName;
	private boolean ack;
	
	public Sender(int projectId, String correspondentName, boolean ack)
	{
		super();
		this.projectId = projectId;
		this.correspondentName = correspondentName;
		this.ack = ack;
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
	public String getCorrespondentName()
	{
		return correspondentName;
	}

	/**
	 * @param correspondentName the correspondentName to set
	 */
	public void setCorrespondentName(String correspondentName)
	{
		this.correspondentName = correspondentName;
	}

	/**
	 * @return the ack
	 */
	public boolean isAck()
	{
		return ack;
	}

	/**
	 * @param ack the ack to set
	 */
	public void setAck(boolean ack)
	{
		this.ack = ack;
	}

}
