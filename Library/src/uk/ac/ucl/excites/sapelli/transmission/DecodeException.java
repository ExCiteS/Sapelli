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

package uk.ac.ucl.excites.sapelli.transmission;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class DecodeException extends Exception
{
	
	private static final long serialVersionUID = 9169900654612685777L;
	
	private Schema schema;
	private List<Record> records; //records at least partially decoded
	
	public DecodeException(String message, Throwable cause, Schema schema, List<Record> records)
	{
		super(message, cause);
		this.schema = schema;
		this.records = new ArrayList<Record>(records);
	}

	public void addRecord(Record record)
	{
		if(record != null)
			this.records.add(record);
	}

	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}

	/**
	 * @return the records
	 */
	public List<Record> getRecords()
	{
		return records;
	}
	
}
