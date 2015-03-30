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

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Abstract class representing a SQLite prepared (or compiled) statement.
 * 
 * Support for SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD") is mandatory.
 * Support for SQL SELECT queries (i.e. "R" from "CRUD") is optional (but unsupported by default).
 * 
 * @author mstevens
 * 
 * @see http://www.sqlite.org/c3ref/stmt.html
 * @see http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
 */
public abstract class SapelliSQLiteStatement
{
	
	protected final List<SQLiteColumn<?, ?>> paramCols;
	
	public SapelliSQLiteStatement()
	{
		this.paramCols = null;
	}
	
	public SapelliSQLiteStatement(List<SQLiteColumn<?, ?>> paramCols)
	{
		this.paramCols = paramCols;
	}
	
	/**
	 * @param record
	 * @throws DBException
	 */
	public void retrieveAndBindAll(Record record) throws DBException
	{
		if(paramCols != null)
		{
			int p = 0;
			for(SQLiteColumn<?, ?> sqliteCol : paramCols)
				sqliteCol.retrieveAndBind(this, p++, record);
		}
	}
	
	/**
	 * @param arguments
	 * @throws DBException
	 */
	public void bindAll(List<Object> arguments) throws DBException
	{
		if(paramCols != null)
		{
			int p = 0;
			for(SQLiteColumn<?, ?> sqliteCol : paramCols)
				sqliteCol.bindSapelliObject(this, p, arguments.get(p++));
		}
	}
	
	public abstract void bindBlob(int paramIdx, byte[] value) throws DBException;
	
	public abstract void bindLong(int paramIdx, Long value) throws DBException;
	
	public abstract void bindDouble(int paramIdx, Double value) throws DBException;
	
	public abstract void bindString(int paramIdx, String value) throws DBException;
	
	public abstract void bindNull(int paramIdx) throws DBException;
	
	public abstract void clearAllBindings();
	
	/**
	 * Executes a SQL INSERT operation, i.e. the creation (the "C" in "CRUD") of a new record in a database table.
	 * 
	 * @return the ROWID of the new record
	 * @throws DBException
	 * 
	 * @see http://www.sqlite.org/lang_createtable.html#rowid
	 * @see http://www.sqlite.org/autoinc.html
	 * @see http://www.sqlite.org/version3.html
	 */
	public abstract long executeInsert() throws DBException;
	
	/**
	 * Executes a SQL UPDATE operation, i.e. the updating (the "U" in "CRUD") of (an) existing record(s) in a database table.
	 * 
	 * @return the number of affected rows 
	 * @throws DBException
	 */
	public abstract int executeUpdate() throws DBException;
	
	/**
	 * Executes a SQL DELETE operation, i.e. the deleting (the "D" in "CRUD") of (an) existing record(s) in a database table.
	 * 
	 * @return the number of affected rows
	 * @throws DBException
	 */
	public abstract int executeDelete() throws DBException;
	
	/**
	 * Executes a SQL SELECT row query, i.e. the reading (the "R" in "CRUD") of (partial) records from a database table.
	 * The results (0, 1 or more rows) are made accessible through a returned {@link ISQLiteCursor} instance.
	 * 
	 * *Not* supported unless subclass overrides this method, otherwise a {@link UnsupportedOperationException} will be thrown.
	 *
	 * @return an {@link ISQLiteCursor} to iterate over results
	 * @throws DBException
	*/
	public ISQLiteCursor executeSelectRows() throws DBException
	{
		throw new UnsupportedOperationException("executeR() is not supported on " + this.getClass().getName()); // !!!
	}
	
	/**
	 * Executes a SQL SELECT query which results in a single (1x1) integer value.
	 * E.g.: "SELECT COUNT(*) FROM SomeTable;"
	 * 
	 * @return the result of the query or null if it returned 0 rows
	 * @throws DBException
	 */
	public abstract Long executeLongQuery() throws DBException;
	
	/**
	 * Releases resources, statement is no longer usable afterwards.
	 */
	public abstract void close();

}
