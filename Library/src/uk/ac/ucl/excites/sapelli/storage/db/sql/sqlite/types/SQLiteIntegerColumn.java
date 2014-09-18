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

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;


/**
 * @author mstevens
 *
 */
public class SQLiteIntegerColumn<SapType> extends SQLiteRecordStore.SQLiteColumn<Long, SapType>
{

	static public final String SQLITE_DATA_TYPE = "INTEGER";
	
	/**
	 * @param store
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(SQLiteRecordStore store, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping)
	{
		store.super(SQLITE_DATA_TYPE, constraint, sourceSchema, sourceColumn, mapping);
	}

	/**
	 * @param store
	 * @param name
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(SQLiteRecordStore store, String name, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping)
	{
		store.super(name, SQLITE_DATA_TYPE, constraint, sourceSchema, sourceColumn, mapping);
	}
	
	/**
	 * For boolean
	 * 
	 * @param store
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 * @param type
	 */
	/*package*/ SQLiteIntegerColumn(SQLiteRecordStore store, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping, String type)
	{
		store.super(type, constraint, sourceSchema, sourceColumn, mapping);
	}

	/**
	 * For boolean
	 * 
	 * @param store
	 * @param name
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 * @param type
	 */
	/*package*/ SQLiteIntegerColumn(SQLiteRecordStore store, String name, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<Long, SapType> mapping, String type)
	{
		store.super(name, type, constraint, sourceSchema, sourceColumn, mapping);
	}

	/**
	 * @param statement
	 * @param paramIdx
	 * @param value non-null
	 */
	@Override
	protected void bindNonNull(SapelliSQLiteStatement statement, int paramIdx, Long value) throws DBException
	{
		statement.bindLong(paramIdx, value);
	}

	@Override
	protected Long getValue(ISQLiteCursor cursor, int columnIdx) throws DBException
	{
		return cursor.getLong(columnIdx);
	}

}
