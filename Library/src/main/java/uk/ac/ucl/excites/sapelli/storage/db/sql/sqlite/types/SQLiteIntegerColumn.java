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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;


/**
 * @author mstevens
 *
 */
public class SQLiteIntegerColumn<SapType> extends SQLiteRecordStore.SQLiteColumn<Long, SapType>
{

	static public final String SQLITE_DATA_TYPE = "INTEGER";
	
	/**
	 * @param store
	 * @param sourceColumnPointer
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(SQLiteRecordStore store, ColumnPointer<? extends Column<SapType>> sourceColumnPointer, TypeMapping<Long, SapType> mapping)
	{
		this(store, null, sourceColumnPointer, mapping);
	}

	/**
	 * @param store
	 * @param name
	 * @param sourceColumnPointer
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteIntegerColumn(SQLiteRecordStore store, String name, ColumnPointer<? extends Column<SapType>> sourceColumnPointer, TypeMapping<Long, SapType> mapping)
	{
		store.super(name, SQLITE_DATA_TYPE, sourceColumnPointer, mapping);
	}

	/**
	 * For boolean
	 * 
	 * @param store
	 * @param name
	 * @param sourceColumnPointer
	 * @param mapping - may be null in case SQLType = SapType
	 * @param type
	 */
	/*package*/ SQLiteIntegerColumn(SQLiteRecordStore store, String name, ColumnPointer<? extends Column<SapType>> sourceColumnPointer, TypeMapping<Long, SapType> mapping, String type)
	{
		store.super(name, type, sourceColumnPointer, mapping);
	}

	/**
	 * @param statement
	 * @param paramIdx
	 * @param value non-null
	 */
	@Override
	protected void bindNonNull(SQLiteStatement statement, int paramIdx, Long value) throws DBException
	{
		statement.bindLong(paramIdx, value);
	}

	@Override
	protected Long getValue(SQLiteCursor cursor, int columnIdx) throws DBException
	{
		return cursor.getLong(columnIdx);
	}

}
