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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite;

import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 * @param <SQLType>
 * @param <SapType>
 */
public abstract class SQLiteColumn<SQLType, SapType> extends SQLColumn<SQLType, SapType>
{

	/**
	 * @param type
	 * @param constraint
	 * @param sourceSchema
	 * @param sourceColumn
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteColumn(String type, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<SQLType, SapType> mapping)
	{
		this(sourceColumn.getName(), type, constraint, new ColumnPointer(sourceSchema, sourceColumn), mapping);
	}
	
	/**
	 * @param name
	 * @param type
	 * @param constraint
	 * @param sourceColumnPointer
	 * @param mapping - may be null in case SQLType = SapType
	 */
	public SQLiteColumn(String name, String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
	{
		super(name, type, constraint, sourceColumnPointer, mapping);
	}
	
	/**
	 * @param statement
	 * @param paramIdx
	 * @param record
	 */
	public void retrieveAndBind(ISQLiteStatement statement, int paramIdx, Record record)
	{
		bind(statement, paramIdx, retrieve(record));
	}
	
	/**
	 * @param paramIdx
	 * @param column
	 * @param value
	 */
	public void bind(ISQLiteStatement statement, int paramIdx, SQLType value)
	{
		if(value != null)
			bindNonNull(statement, paramIdx, value);
		else
			statement.bindNull(paramIdx);
	}
	
	protected abstract void bindNonNull(ISQLiteStatement statement, int paramIdx, SQLType value);

	@Override
	protected String getNullString()
	{
		return "null";
	}

	@Override
	protected String getQuoteChar()
	{
		return "'";
	}

	@Override
	protected String getQuoteEscape()
	{
		return "''";
	}
	
	/**
	 * @param record
	 * @param cursor
	 * @param columnIdx
	 */
	public void storeFrom(Record record, ISQLiteCursor cursor, int columnIdx)
	{
		store(record, readFrom(cursor, columnIdx));
	}
	
	public SQLType readFrom(ISQLiteCursor cursor, int columnIdx)
	{
		if(cursor.isNull(columnIdx))
			return null;
		return getFrom(cursor, columnIdx);
	}
	
	protected abstract SQLType getFrom(ISQLiteCursor cursor, int columnIdx);

}
