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
import com.almworks.sqlite4java.SQLiteConstants;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

import uk.ac.ucl.excites.sapelli.shared.db.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement;

/**
 * A sqlite4java-specific {@link SapelliSQLiteStatement} subclass. It is a thin wrapper around sqlite4java's {@link SQLiteStatement} class.
 * 
 * Supports SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD"), as well as SQL SELECT row queries (i.e. "R" from "CRUD") and
 * "simple" SELECT queries resulting in a 1x1 long value. In addition it also acts as a cursor and thus also implements the {@link ISQLiteCursor} interface.
 * 
 * @author mstevens
 *
 * @see com.almworks.sqlite4java.SQLiteStatement
 */
public class JavaSQLiteStatement extends SapelliSQLiteStatement implements ISQLiteCursor
{

	private final SQLiteConnection db;
	private final SQLiteStatement javaSQLiteSt;
	private Boolean firstStep = null;
	
	/**
	 * @param javaSQLiteSt
	 * @param paramCols
	 * @throws DBException
	 */
	public JavaSQLiteStatement(SQLiteConnection db, SQLiteStatement javaSQLiteSt, List<SQLiteColumn<?, ?>> paramCols) throws SQLiteException
	{
		super(paramCols);
		this.db = db;
		this.javaSQLiteSt = javaSQLiteSt; // not necessarily a new SQLiteStatement (may have been cached), therefore...
		// ... reset & clear bindings if necessary:
		if(javaSQLiteSt.hasStepped() || javaSQLiteSt.hasBindings())
			javaSQLiteSt.reset(true);
	}
	
	/**
	 * @param arguments
	 * @throws DBException
	 */
	public void bindAll(List<Object> arguments) throws DBException
	{
		if(paramCols != null)
		{
			for(int p = 0, count = paramCols.size(); p < count; p++)
				paramCols.get(p).bindSapelliObject(this, p, arguments.get(p));
		}
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

	/**
	 * Clears all existing bindings
	 * 
	 * If the statement has stepped we also do a reset. This is because other wise subsequent calls to bind* methods result in errors.
	 * The reason is that (copied from SQLite C interface docs) "if any of the sqlite3_bind_*() routines are called [...] with a prepared
	 * statement for which sqlite3_step() has been called more recently than sqlite3_reset(), then the call will return SQLITE_MISUSE."
	 * 
	 * @see http://www.sqlite.org/c3ref/bind_blob.html
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#clearAllBindings()
	 */
	@Override
	public void clearAllBindings() throws DBException
	{
		try
		{
			if(javaSQLiteSt.hasStepped())
				javaSQLiteSt.reset(true); // also clears bindings
			else
				javaSQLiteSt.clearBindings();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Exception upon clearing statement bindings", e);
		}
	}
	
	/**
	 * Insert a record
	 *  
	 * @throws DBException 
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#executeInsert()
	 */
	@Override
	public long executeInsert() throws DBException
	{
		try
		{
			javaSQLiteSt.step();
			return db.getLastInsertId();
		}
		catch(SQLiteException e)
		{
			if(e.getErrorCode() == SQLiteConstants.SQLITE_CONSTRAINT)
				throw new DBConstraintException("Failed to execute INSERT statement due to constraint violation", e);
			throw new DBException("Failed to execute INSERT statement", e);
		}
	}

	/**
	 * Update a record
	 *   
	 * @throws DBException 
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#executeUpdate()
	 */
	@Override
	public int executeUpdate() throws DBException
	{
		try
		{
			javaSQLiteSt.step();
			return db.getChanges();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute UPDATE statement", e);
		}
	}

	/**
	 * Delete a record
	 *  
	 * @throws DBException 
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#executeDelete()
	 */
	@Override
	public int executeDelete() throws DBException
	{
		try
		{
			javaSQLiteSt.step();
			return db.getChanges();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute DELETE statement", e);
		}
	}

	@Override
	public Long executeLongQuery() throws DBException
	{
		try
		{
			if(javaSQLiteSt.hasStepped())
				javaSQLiteSt.reset(false);
			if(javaSQLiteSt.step())
				return javaSQLiteSt.columnLong(0);
			else
				return null;
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to execute simple long query", e);
		}
	}
	
	/**
	 * Executes a SQL SELECT row query, i.e. the reading (the "R" in "CRUD") of (partial) records from a database table.
	 * The results (0, 1 or more rows) are made accessible through a returned {@link ISQLiteCursor} instance.
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
		try
		{
			if(javaSQLiteSt.hasStepped())
			{
				firstStep = null;
				javaSQLiteSt.reset(false); // don't clear bindings!
			}
			firstStep = moveToNext();
			return this; // act as cursor
		}
		catch(SQLiteException /* from javaSQLiteSt.reset(boolean) */ | DBException /* from moveToNext(), we could throw this as-is but msg is confusing */ e)
		{
			throw new DBException("Failed to execute SELECT rows query", e instanceof DBException ? e.getCause() : e);
		}
	}
	
	@Override
	public boolean moveToNext() throws DBException
	{
		try
		{
			if(firstStep != null)
			{
				boolean holdFirstStep = firstStep;
				firstStep = null; // !!!
				return holdFirstStep;
			}
			else
				return javaSQLiteSt.step();
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to move cursor to next position", e);
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

	@Override
	public void close()
	{
		javaSQLiteSt.dispose(); // Although it is somewhat counterintuitive this allows the prepared statement to be returned to the cache for future re-use
	}
	
	@Override
	public boolean isClosed()
	{
		return javaSQLiteSt.isDisposed();
	}
	
}
