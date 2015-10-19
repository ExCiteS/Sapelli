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

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;

/**
 * A sqlite4java-specific {@link SQLiteStatement} subclass. It is a thin wrapper around sqlite4java's {@link SQLiteStatement} class.
 * 
 * Supports SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD"), as well as SQL SELECT row queries (i.e. "R" from "CRUD") and
 * "simple" SELECT queries resulting in a 1x1 long value. In addition it also acts as a cursor and thus also implements the {@link SQLiteCursor} interface.
 * 
 * @author mstevens
 *
 * @see com.almworks.sqlite4java.SQLiteStatement
 */
public class JavaSQLiteStatement extends SQLiteStatement implements SQLiteCursor
{

	private final SQLiteConnection db;
	private final com.almworks.sqlite4java.SQLiteStatement javaSQLiteSt;
	private Boolean firstStep = null;

	/**
	 * @param db
	 * @param sql
	 * @param paramCols
	 * @throws SQLiteException
	 * @throws DBException
	 */
	public JavaSQLiteStatement(SQLiteConnection db, String sql, List<SQLiteColumn<?, ?>> paramCols) throws SQLiteException, DBException
	{
		super(paramCols);
		this.db = db;
		this.javaSQLiteSt = db.prepare(sql, true); // not necessarily a new SQLiteStatement (may have been cached), therefore...
		// ... clear bindings (& reset) if necessary:
		clearAllBindings(); // will also call reset() if the statement "has stepped"
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
	 * Only serves to "eat" exceptions thrown by {@link SQLiteStatement#reset(boolean)}, which are typically
	 * "re-throws" of failures during earlier executions of the statement.
	 * 
	 * @param clearBindings
	 */
	protected void reset(boolean clearBindings)
	{
		try
		{
			javaSQLiteSt.reset(clearBindings && javaSQLiteSt.hasBindings()); // also clear bindings if there are any
		}
		catch(SQLiteException e)
		{
			System.err.println("Exception upon statement reset (probably caused by failure during earlier execution, and hence ignorable):");
			e.printStackTrace(System.err);
		}
	}
	
	/**
	 * Clears all existing bindings.
	 * 
	 * If the statement has stepped we also do a reset. This is because otherwise subsequent calls to bind* methods result in errors.
	 * The reason is that (copied from SQLite C interface docs) "if any of the sqlite3_bind_*() routines are called [...] with a prepared
	 * statement for which sqlite3_step() has been called more recently than sqlite3_reset(), then the call will return SQLITE_MISUSE."
	 * 
	 * Any exceptions thrown by {@link SQLiteStatement#reset(boolean)} or {@link SQLiteStatement#clearBindings()} are "eaten" because
	 * these are typically "re-throws" of failures during earlier executions of the statement.
	 * 
	 * @see http://www.sqlite.org/c3ref/bind_blob.html
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#clearAllBindings()
	 */
	@Override
	public void clearAllBindings()
	{
		if(javaSQLiteSt.hasStepped())
			reset(true); // also clears bindings if there are any
		else if(javaSQLiteSt.hasBindings())
			try
			{
				javaSQLiteSt.clearBindings();
			}
			catch(SQLiteException e)
			{
				System.err.println("Exception upon clearing statement bindings (probably caused by failure during earlier execution, and hence ignorable):");
				e.printStackTrace(System.err);
			}
	}
	
	/**
	 * Insert a record
	 * 
	 * @throws DBPrimaryKeyException
	 * @throws DBConstraintException
	 * @throws DBException
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeInsert()
	 */
	@Override
	public long executeInsert() throws DBPrimaryKeyException, DBConstraintException, DBException
	{
		try
		{
			javaSQLiteSt.step();
			long rowID = db.getLastInsertId();
			if(rowID <= 0)
				throw new DBException(formatMessageWithSQL("Execution of INSERT statement (%s) failed (returned ROWID = " + rowID + ")"));
			return rowID;
		}
		catch(SQLiteException e)
		{
			if(e.getBaseErrorCode() == SQLiteConstants.SQLITE_CONSTRAINT)
			{
				String msg = e.getMessage();
				if(msg != null && msg.toUpperCase().contains("PRIMARY KEY"))
					throw new DBPrimaryKeyException(formatMessageWithSQL("Failed to execute INSERT statement (%s) due to existing record with same primary key"), e);
				else
					throw new DBConstraintException(formatMessageWithSQL("Failed to execute INSERT statement (%s) due to constraint violation"), e);
			}
			throw new DBException(formatMessageWithSQL("Failed to execute INSERT statement: %s"), e);
		}
	}

	/**
	 * Update a record
	 * 
	 * @throws DBConstraintException
	 * @throws DBException 
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeUpdate()
	 */
	@Override
	public int executeUpdate() throws DBConstraintException, DBException
	{
		try
		{
			javaSQLiteSt.step();
			return db.getChanges();
		}
		catch(SQLiteException e)
		{
			if(e.getBaseErrorCode() == SQLiteConstants.SQLITE_CONSTRAINT)
				throw new DBConstraintException(formatMessageWithSQL("Failed to execute UPDATE statement (%s) due to constraint violation"), e);
			throw new DBException(formatMessageWithSQL("Failed to execute UPDATE statement: %s"), e);
		}
	}

	/**
	 * Delete a record
	 *  
	 * @throws DBException 
	 * 
	 * @see http://almworks.com/sqlite4java/javadoc/com/almworks/sqlite4java/SQLiteStatement.html#step()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeDelete()
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
			throw new DBException(formatMessageWithSQL("Failed to execute DELETE statement: %s"), e);
		}
	}

	@Override
	public Long executeLongQuery() throws DBException
	{
		try
		{
			if(javaSQLiteSt.hasStepped())
				reset(false); // don't clear bindings!
			if(javaSQLiteSt.step())
				return javaSQLiteSt.columnLong(0);
			else
				return null;
		}
		catch(SQLiteException e)
		{
			throw new DBException(formatMessageWithSQL("Failed to execute simple long query: %s"), e);
		}
	}
	
	/**
	 * Executes a SQL SELECT row query, i.e. the reading (the "R" in "CRUD") of (partial) records from a database table.
	 * The results (0, 1 or more rows) are made accessible through a returned {@link SQLiteCursor} instance.
	 * 
	 * @return an {@link SQLiteCursor} (effectively "this") to iterate over results
	 * @throws DBException
	 * 
	 * @see http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeSelectRows()
	 */
	@Override
	public SQLiteCursor executeSelectRows() throws DBException
	{
		if(javaSQLiteSt.hasStepped())
		{
			firstStep = null;
			reset(false); // don't clear bindings!
		}
		try
		{
			firstStep = moveToNext();
			return this; // act as cursor
		}
		catch(DBException e) // from moveToNext(), we could throw this as-is but msg is confusing
		{
			throw new DBException(formatMessageWithSQL("Failed to execute SELECT rows query: %s"), e.getCause());
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
		javaSQLiteSt.dispose(); // Although it is somewhat counter-intuitive this allows the prepared statement to be returned to the cache for future re-use
	}
	
	@Override
	public boolean isClosed()
	{
		return javaSQLiteSt.isDisposed();
	}
	
	@Override
	public String toString()
	{
		return javaSQLiteSt.toString();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#getSQL()
	 */
	@Override
	protected String getSQL()
	{
		return javaSQLiteSt.getSqlParts().toString();
	}
	
}
