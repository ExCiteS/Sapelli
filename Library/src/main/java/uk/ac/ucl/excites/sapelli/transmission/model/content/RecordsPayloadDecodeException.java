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

package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;

/**
 * @author mstevens
 *
 */
public class RecordsPayloadDecodeException extends PayloadDecodeException
{
	
	private static final long serialVersionUID = 2L;
		
	/**
	 * Partially decoded records (not stored in the RecordPayload itself)
	 */
	private List<Record> partialRecords;
	
	public RecordsPayloadDecodeException(RecordsPayload payload, Throwable cause)
	{
		this(payload, "Error upon decoding RecordsPayload", cause);
	}
	
	public RecordsPayloadDecodeException(RecordsPayload payload, String message)
	{
		this(payload, message, null);
	}
	
	public RecordsPayloadDecodeException(RecordsPayload payload, String message, Throwable cause)
	{
		super(payload, message, cause);
		this.partialRecords = new ArrayList<Record>();
	}

	public void addPartialRecord(Record record)
	{
		if(record != null)
			this.partialRecords.add(record);
	}

	/**
	 * @return the modelID
	 */
	public long getModelID()
	{
		return ((RecordsPayload) payload).model.getID();
	}

	/**
	 * @return the records
	 */
	public List<Record> getRecords()
	{
		List<Record> records = new ArrayList<Record>();
		records.addAll(((RecordsPayload) payload).getRecords());
		records.addAll(partialRecords);
		return records;
	}
	
}
