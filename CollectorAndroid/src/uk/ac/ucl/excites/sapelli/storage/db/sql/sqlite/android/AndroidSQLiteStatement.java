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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.android;

import java.util.List;

import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.database.SQLException;
import android.database.sqlite.SQLiteConstraintException;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteFullException;
import android.os.Build;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.storage.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;

/**
 * An Android-specific {@link SQLiteStatement} subclass. It is a thin wrapper around Android's {@link SQLiteStatement} class.
 * 
 * Supports SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD"). SQL SELECT row queries are not supported,
 * but "simple" SELECT queries resulting in a 1x1 long value are.
 * 
 * @author mstevens
 *
 * @see android.database.sqlite.SQLiteDatabase
 * @see android.database.sqlite.SQLiteStatement
 */
public class AndroidSQLiteStatement extends SQLiteStatement
{
	
	private final AndroidSQLiteRecordStore recordStore;
	private final android.database.sqlite.SQLiteStatement androidSQLiteSt;
	
	/**
	 * @param androidSQLiteRecordStore
	 * @param androidSQLiteSt
	 * @param paramCols
	 * @throws SQLException
	 */
	public AndroidSQLiteStatement(AndroidSQLiteRecordStore recordStore, android.database.sqlite.SQLiteStatement androidSQLiteSt, List<SQLiteColumn<?, ?>> paramCols)
	{
		super(paramCols);
		this.recordStore = recordStore;
		this.androidSQLiteSt = androidSQLiteSt;
	}
	
	@Override
	public void bindBlob(int paramIdx, byte[] value)
	{
		androidSQLiteSt.bindBlob(paramIdx, value);
	}

	@Override
	public void bindLong(int paramIdx, Long value)
	{
		androidSQLiteSt.bindLong(paramIdx, value);
	}

	@Override
	public void bindDouble(int paramIdx, Double value)
	{
		androidSQLiteSt.bindDouble(paramIdx, value);
	}

	@Override
	public void bindString(int paramIdx, String value)
	{
		androidSQLiteSt.bindString(paramIdx, value);
	}

	@Override
	public void bindNull(int paramIdx)
	{
		androidSQLiteSt.bindNull(paramIdx);
	}

	@Override
	public void clearAllBindings()
	{
		androidSQLiteSt.clearBindings();
	}

	/**
	 * Note:
	 * 	{@link android.database.sqlite.SQLiteStatement#executeInsert()} supposedly returns -1 if nothing is inserted,
	 * 	but looking at the implementation I think that's a documentation bug and it should have been 0.
	 *
	 * @see https://code.google.com/p/android/issues/detail?id=199493
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement#executeInsert(Long)
	 */
	@SuppressLint("DefaultLocale")
	@Override
	public long executeInsert() throws DBPrimaryKeyException, DBConstraintException, DBException
	{
		verifyLastInsert = false;
		try
		{
			long lastRowID = androidSQLiteSt.executeInsert(); // return 0 (or -1??) if nothing is inserted
			if(lastRowID == 0 || lastRowID == -1) // a ROWID of 0 or -1 can be valid in a number of cases, but executeInsert() also returns 0 (or -1??) if nothing was inserted... 
				verifyLastInsert = true; // so this insert must be verified
			return lastRowID;
		}
		catch(SQLiteConstraintException sqliteConstrE)
		{
			String msg = sqliteConstrE.getMessage();
			if(msg != null && msg.toUpperCase().contains("PRIMARY KEY"))
				throw new DBPrimaryKeyException(formatMessageWithSQL("Failed to execute INSERT statement (%s) due to existing record with same primary key"), sqliteConstrE);
			else
				throw new DBConstraintException(formatMessageWithSQL("Failed to execute INSERT statement (%s) due to constraint violation"), sqliteConstrE);
		}
		catch(SQLiteFullException sqliteFullE)  // happens (among other cases) when an auto-incrementing PK has reached max value (9223372036854775807) in previous insert
		{
			throw new DBException(formatMessageWithSQL("Failed to execute INSERT statement (%s) due to table, database or storage medium being full"), sqliteFullE);	
		}
		catch(SQLException sqlE)
		{
			throw new DBException(formatMessageWithSQL("Failed to execute INSERT statement: %s"), sqlE);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeUpdate()
	 */
	@Override
	public int executeUpdate() throws DBConstraintException, DBException
	{
		try
		{
			return executeUpdateDelete();
		}
		catch(SQLiteConstraintException sqliteConstrE)
		{
			throw new DBConstraintException(formatMessageWithSQL("Failed to execute UPDATE statement (%s) due to constraint violation"), sqliteConstrE);
		}
		catch(SQLException sqlE)
		{
			throw new DBException(formatMessageWithSQL("Failed to execute UPDATE statement: %s"), sqlE);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#executeDelete()
	 */
	@Override
	public int executeDelete() throws DBException
	{
		try
		{
			return executeUpdateDelete();
		}
		catch(SQLException sqlE)
		{
			throw new DBException(formatMessageWithSQL("Failed to execute DELETE statement: %s"), sqlE);
		}
	}
	
	private final int executeUpdateDelete() throws SQLException, DBException
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
			// API levels 11 and higher
			return executeUpdateDeleteNew();
		else
		{	// API levels 1 to 10
			androidSQLiteSt.execute();						// execute the operation itself
			return recordStore.getNumberOfAffectedRows();	// determine number of affected rows
		}
	}
	
	@TargetApi(Build.VERSION_CODES.HONEYCOMB)
	private final int executeUpdateDeleteNew() throws SQLException
	{
		return androidSQLiteSt.executeUpdateDelete();
	}
	
	@Override
	public Long executeLongQuery() throws DBException
	{
		try
		{
			return androidSQLiteSt.simpleQueryForLong();
		}
		catch(SQLiteDoneException e)
		{
			return null;
		}
	}

	@Override
	public void close()
	{
		androidSQLiteSt.close();
	}
	
	/**
	 * @return SQL expression
	 * @see android.database.sqlite.SQLiteStatement#toString()
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement#getSQL()
	 */
	@Override
	protected String getSQL()
	{
		return androidSQLiteSt.toString().substring("SQLiteProgram: ".length());
	}
	
}
