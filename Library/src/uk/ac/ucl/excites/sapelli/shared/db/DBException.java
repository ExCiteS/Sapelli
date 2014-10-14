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

package uk.ac.ucl.excites.sapelli.shared.db;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class DBException extends Exception
{

	private List<Record> records = null;
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2L;

	/**
	 * @param detailMessage
	 * @param cause
	 */
	public DBException(String detailMessage, Throwable cause)
	{
		this(detailMessage, cause, (Record[]) null);
	}
	
	/**
	 * @param detailMessage
	 * @param cause
	 * @param records
	 */
	public DBException(String detailMessage, Throwable cause, Record... records)
	{
		super(detailMessage, cause);
		addRecords(records);
	}

	/**
	 * @param detailMessage
	 */
	public DBException(String detailMessage)
	{
		this(detailMessage, (Record[]) null);
	}
	
	/**
	 * @param detailMessage
	 * @param records
	 */
	public DBException(String detailMessage, Record... records)
	{
		super(detailMessage);
		addRecords(records);
	}

	/**
	 * @param cause
	 */
	public DBException(Throwable cause)
	{
		this(cause, (Record[]) null);
	}
	
	/**
	 * @param cause
	 * @param records
	 */
	public DBException(Throwable cause, Record... records)
	{
		super(cause);
		addRecords(records);
	}
	
	private void addRecords(Record... records)
	{
		if(records == null || records.length == 0)
			return;
		if(this.records == null)
			this.records = new ArrayList<Record>();
		for(Record r : records)
			if(r != null)
				this.records.add(r);
	}
	
	public List<Record> getRecords()
	{
		return this.records == null ? Collections.<Record> emptyList() : this.records;
	}

}
