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

package uk.ac.ucl.excites.sapelli.storage.db.sql;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 * @param <T>
 */
public abstract class StoredColumn<T>
{
	
	final ColumnPointer sourceColumnPointer;
	final String name;
	final String spec;
	
	public StoredColumn(Schema schema, Column<T> sourceColum, String name, String spec)
	{
		this.sourceColumnPointer = new ColumnPointer(schema, sourceColum);
		this.name = name;
		this.spec = spec;
	}
	
	@SuppressWarnings("unchecked")
	public String getStorableValueString(Record record)
	{
		return toStorableString((T) sourceColumnPointer.retrieveValue(record));
	}
	
	@SuppressWarnings("unchecked")
	public String getStorableValueString(Object value)
	{
		return toStorableString((T) value);
	}
	
	public String toStorableString(T value)
	{
		if(value == null)
			return getNullString();
		else
			return toStorableValueString(value);
	}
	
	public abstract String getNullString();
	
	/**
	 * @param value (guaranteed non-null)
	 * @return
	 */
	public abstract String toStorableValueString(T value);
	
}