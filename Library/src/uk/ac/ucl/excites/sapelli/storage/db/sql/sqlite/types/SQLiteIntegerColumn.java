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
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteStatement;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;


/**
 * @author mstevens
 *
 */
public class SQLiteIntegerColumn<SapType> extends SQLiteColumn<Long, SapType>
{

	static public final String SQLITE_DATA_TYPE = "INTEGER";
	
	/**
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping)
	{
		super(SQLITE_DATA_TYPE, constraint, sourceSchema, sourceColumn, mapping);
	}

	/**
	 * @param name
	 * @param constraint
	 * @param sourceColumnPointer
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(String name, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<Long, SapType> mapping)
	{
		super(name, SQLITE_DATA_TYPE, constraint, sourceColumnPointer, mapping);
	}
	
	/**
	 * For boolean
	 * 
	 * @param type
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping
	 */
	/*package*/ SQLiteIntegerColumn(String type, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping)
	{
		super(type, constraint, sourceSchema, sourceColumn, mapping);
	}

	/**
	 * For boolean
	 * 
	 * @param name
	 * @param type
	 * @param constraint
	 * @param sourceColumnPointer
	 * @param mapping
	 */
	/*package*/ SQLiteIntegerColumn(String name, String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<Long, SapType> mapping)
	{
		super(name, type, constraint, sourceColumnPointer, mapping);
	}

	/**
	 * @param statement
	 * @param paramIdx
	 * @param value non-null
	 */
	@Override
	protected void bindNonNull(ISQLiteStatement statement, int paramIdx, Long value)
	{
		statement.bindLong(paramIdx, value);
	}

	@Override
	protected Long getFrom(ISQLiteCursor cursor, int columnIdx)
	{
		return cursor.getLong(columnIdx);
	}

}
