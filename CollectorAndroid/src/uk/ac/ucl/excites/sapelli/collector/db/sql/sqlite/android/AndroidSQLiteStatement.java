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

package uk.ac.ucl.excites.sapelli.collector.db.sql.sqlite.android;

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SapelliSQLiteStatement;
import android.annotation.TargetApi;
import android.database.SQLException;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteStatement;
import android.os.Build;

/**
 * An Android-specific {@link SapelliSQLiteStatement} subclass. It is a thin wrapper around Android's {@link SQLiteStatement} class.
 * 
 * Supports SQL INSERT, UPDATE, and DELETE operations (i.e. "CUD" from "CRUD"). SQL SELECT row queries are not supported,
 * but "simple" SELECT queries resulting in a 1x1 long value are.
 * 
 * @author mstevens
 *
 * @see android.database.sqlite.SQLiteDatabase
 * @see android.database.sqlite.SQLiteStatement
 */
public class AndroidSQLiteStatement extends SapelliSQLiteStatement
{
	
	private final AndroidSQLiteRecordStore recordStore;
	private final SQLiteStatement androidSQLiteSt;
	
	/**
	 * @param androidSQLiteRecordStore
	 * @param androidSQLiteSt
	 * @param paramCols
	 * @throws SQLException
	 */
	public AndroidSQLiteStatement(AndroidSQLiteRecordStore recordStore, SQLiteStatement androidSQLiteSt, List<SQLiteColumn<?, ?>> paramCols)
	{
		super(paramCols);
		this.recordStore = recordStore;
		this.androidSQLiteSt = androidSQLiteSt;
	}
	
	@Override
	public void bindBlob(int paramIdx, byte[] value)
	{
		androidSQLiteSt.bindBlob(paramIdx + 1, value); // SQLiteProgram uses 1-based parameter indexes 
	}

	@Override
	public void bindLong(int paramIdx, Long value)
	{
		androidSQLiteSt.bindLong(paramIdx + 1, value); // SQLiteProgram uses 1-based parameter indexes
	}

	@Override
	public void bindDouble(int paramIdx, Double value)
	{
		androidSQLiteSt.bindDouble(paramIdx + 1, value); // SQLiteProgram uses 1-based parameter indexes
	}

	@Override
	public void bindString(int paramIdx, String value)
	{
		androidSQLiteSt.bindString(paramIdx + 1, value); // SQLiteProgram uses 1-based parameter indexes
	}

	@Override
	public void bindNull(int paramIdx)
	{
		androidSQLiteSt.bindNull(paramIdx + 1); // SQLiteProgram uses 1-based parameter indexes
	}

	@Override
	public void clearAllBindings()
	{
		androidSQLiteSt.clearBindings();
	}

	@Override
	public long executeInsert() throws DBException
	{
		try
		{
			long rowID = androidSQLiteSt.executeInsert();
			if(rowID == -1)
				throw new DBException("Execution of INSERT statement failed (returned ROWID = -1)");
			return rowID;
		}
		catch(SQLException sqlE)
		{
			throw new DBException("Failed to execute INSERT statement", sqlE);
		}
	}

	@Override
	public int executeUpdate() throws DBException
	{
		try
		{
			return executeUpdateDelete();
		}
		catch(SQLException sqlE)
		{
			throw new DBException("Failed to execute UPDATE statement", sqlE);
		}
	}

	@Override
	public int executeDelete() throws DBException
	{
		try
		{
			return executeUpdateDelete();
		}
		catch(SQLException sqlE)
		{
			throw new DBException("Failed to execute DELETE statement", sqlE);
		}
	}
	
	private int executeUpdateDelete() throws SQLException
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
	private int executeUpdateDeleteNew() throws SQLException
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
	
}
