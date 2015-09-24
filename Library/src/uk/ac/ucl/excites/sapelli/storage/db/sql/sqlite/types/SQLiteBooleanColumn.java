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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types;

import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;


/**
 * @author mstevens
 *
 */
public class SQLiteBooleanColumn extends SQLiteIntegerColumn<Boolean>
{

	static public final String SQLITE_DATA_TYPE = "BOOLEAN";
	
	static private final TypeMapping<Long, Boolean> boolIntMapping = new TypeMapping<Long, Boolean>()
	{
		
		@Override
		public Boolean toSapelliType(Long value)
		{
			return value == 1;
		}
		
		@Override
		public Long toSQLType(Boolean value)
		{
			return value ? 1l : 0l;
		}
	};
	
	/**
	 * @param store
	 * @param sourceColumnPointer
	 */
	public SQLiteBooleanColumn(SQLiteRecordStore store, ColumnPointer<? extends Column<Boolean>> sourceColumnPointer)
	{
		this(store, null, sourceColumnPointer);
	}

	/**
	 * @param store
	 * @param name
	 * @param sourceColumnPointer
	 */
	public SQLiteBooleanColumn(SQLiteRecordStore store, String name, ColumnPointer<? extends Column<Boolean>> sourceColumnPointer)
	{
		super(store, name, sourceColumnPointer, boolIntMapping, SQLITE_DATA_TYPE);
	}
	
}
