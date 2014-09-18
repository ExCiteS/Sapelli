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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.java;

import java.util.List;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement;

/**
 * A sqlite4java-specific {@link SapelliSQLiteStatement} subclass. It is a thin wrapper around sqlite4java's {@link SQLiteStatement} class.
 * 
 * Supports SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD"), as well as SQL SELECT row queries (i.e. "R" from "CRUD")
 * and "simple" SELECT queries resulting in a 1x1 long value. In addition it also acts as a cursor and thus also implements the {@link ISQLiteCursor} interface.
 * 
 * @author mstevens
 *
 * @see com.almworks.sqlite4java.SQLiteStatement
 */
public class JavaSQLiteStatement extends SapelliSQLiteStatement implements ISQLiteCursor
{

	private final SQLiteConnection db;
	private final SQLiteStatement javaSQLiteSt;
	private Boolean execStep = false;
	
	/**
	 * @param javaSQLiteSt
	 * @param paramCols
	 * @throws DBException
	 */
	public JavaSQLiteStatement(SQLiteConnection db, SQLiteStatement javaSQLiteSt, List<SQLiteColumn<?, ?>> paramCols) throws DBException
	{
		super(paramCols);
		this.db = db;
		this.javaSQLiteSt = javaSQLiteSt;
	}

	@Override
	public void bindBlob(int paramIdx, byte[] value) throws DBException
	{
		try
		{
			javaSQLiteSt.bind(paramIdx + 1, value); // SQLite uses 1-based parameter indexes when binding
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon binding blob (byte[]) value to parameter " + paramIdx, e);
		} 
	}

	@Override
	public void bindLong(int paramIdx, Long value) throws DBException
	{
		try
		{
			javaSQLiteSt.bind(paramIdx + 1, value); // SQLite uses 1-based parameter indexes when binding
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon binding Long value to parameter " + paramIdx, e);
		}
	}

	@Override
	public void bindDouble(int paramIdx, Double value) throws DBException
	{
		try
		{
			javaSQLiteSt.bind(paramIdx + 1, value); // SQLite uses 1-based parameter indexes when binding
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon binding Double value to parameter " + paramIdx, e);
		}
	}

	@Override
	public void bindString(int paramIdx, String value) throws DBException
	{
		try
		{
			javaSQLiteSt.bind(paramIdx + 1, value); // SQLite uses 1-based parameter indexes when binding
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon binding String value to parameter " + paramIdx, e);
		}
	}

	@Override
	public void bindNull(int paramIdx) throws DBException
	{
		try
		{
			javaSQLiteSt.bindNull(paramIdx + 1); // SQLite uses 1-based parameter indexes when binding
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon binding null value to parameter " + paramIdx, e);
		}
	}

	@Override
	public void clearAllBindings() throws DBException
	{
		try
		{
			javaSQLiteSt.clearBindings();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon clearing statement bindings", e);
		}
	}
	
	@Override
	public long executeInsert() throws DBException
	{
		try
		{
			// TODO reset if hasstepped?
			javaSQLiteSt.step();
			return db.getLastInsertId();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute INSERT statement", e);
		}
	}

	@Override
	public int executeUpdate() throws DBException
	{
		try
		{
			// TODO reset if hasstepped?
			javaSQLiteSt.step();
			return db.getChanges();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute UPDATE statement", e);
		}
	}

	@Override
	public int executeDelete() throws DBException
	{
		try
		{
			// TODO reset if hasstepped?
			javaSQLiteSt.step();
			return db.getChanges();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute DELETE statement", e);
		}
	}

	@Override
	public long executeLongQuery() throws DBException
	{
		try
		{
			// TODO reset if hasstepped?
			javaSQLiteSt.step();
			if(javaSQLiteSt.hasRow())
				return javaSQLiteSt.columnLong(0);
			else
				throw new DBException("Simple long query returned no results");
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute simple long query", e);
		}
	}
	
	/**
	 * Execute an "R" operation (from "CRUD"), i.e. the Reading (= SELECT)) of a record.
	 * 
	 * @return an {@link ISQLiteCursor} (effectively "this") to iterate over results
	 * @throws DBException
	 * 
	 * @see http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#executeSelectRows()
	 */
	@Override
	public ISQLiteCursor executeSelectRows() throws DBException
	{
		// TODO Auto-generated method stub
	
		// TODO reset if hasstepped?
//		execStep = javaSQLiteSt.step();
//		//return execStep;
		
		return this;
	}
	
	@Override
	public boolean moveToNext()
	{
		// TODO get this right...
		if(execStep)
		{	// first moveToNext() call after execute()
			execStep = false;
			return true;
		}
		try
		{
			return javaSQLiteSt.step();
		}
		catch(SQLiteException e)
		{
			// TODO throw DBException (change signature accordingly)
			e.printStackTrace();
			return false;
		}
	}

	@Override
	public void close()
	{
		try
		{
			javaSQLiteSt.reset();
		}
		catch(SQLiteException e)
		{
			e.printStackTrace(System.err);
		}
	}

	@Override
	public boolean hasRow()
	{
		return javaSQLiteSt.hasRow();
	}

	@Override
	public byte[] getBlob(int columnIdx) throws DBException
	{
		try
		{
			return javaSQLiteSt.columnBlob(columnIdx);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon getting blob (byte[]) value from column " + columnIdx, e);
		}
	}

	@Override
	public long getLong(int columnIdx) throws DBException
	{
		try
		{
			return javaSQLiteSt.columnLong(columnIdx);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon getting long value from column " + columnIdx, e);
		}
	}

	@Override
	public double getDouble(int columnIdx) throws DBException
	{
		try
		{
			return javaSQLiteSt.columnDouble(columnIdx);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon getting double value from column " + columnIdx, e);
		}
	}

	@Override
	public String getString(int columnIdx) throws DBException
	{
		try
		{
			return javaSQLiteSt.columnString(columnIdx);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon getting String value from column " + columnIdx, e);
		}
	}

	@Override
	public boolean isNull(int columnIdx) throws DBException
	{
		try
		{
			return javaSQLiteSt.columnNull(columnIdx);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon checking for null in column " + columnIdx, e);
		}
	}
	
}
