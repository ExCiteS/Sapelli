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
 * Class that embodies a record-receiving relationship for a particular Project, from a particular sending Correspondent.
 * @author benelliott
 */
public class RecordReceival
{
	static public final int SENDER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping SENDER_ID_FIELD = IntegerRangeMapping.ForSize(0, SENDER_ID_SIZE); // unsigned(!) 24 bit integer
	
	private Project project;
	private Correspondent sender;
	private boolean ack;
	
	public RecordReceival(Project project, Correspondent sender, boolean ack)
	{
		this.project = project;
		this.sender = sender;
		this.ack = ack;
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
	 * @return the sender
	 */
	public Correspondent getSender()
	{
		return sender;
	}

	/**
	 * @param sender the sender to set
	 */
	public void setCorrespondent(Correspondent sender)
	{
		this.sender = sender;
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
