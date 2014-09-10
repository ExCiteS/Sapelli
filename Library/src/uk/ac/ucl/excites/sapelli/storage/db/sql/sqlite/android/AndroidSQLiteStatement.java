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

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;

/**
 * @author mstevens
 *
 * @see android.database.sqlite.SQLiteDatabase
 * @see android.database.sqlite.SQLiteProgram
 */
public class AndroidSQLiteStatement extends SQLiteStatement
{

	static public final char PARAM_PLACEHOLDER = '?';
	
	private final android.database.sqlite.SQLiteStatement androidSQLiteSt;
	
	/**
	 * @param sql
	 * @throws SQLException
	 */
	public AndroidSQLiteStatement(SQLiteDatabase db, String sql) throws SQLException
	{
		androidSQLiteSt = db.compileStatement(sql);
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

	/**
	 * This should never happen because bindLiteral() is only called from
	 * SQLColumn#bind() and SQLColumn#bindNull(), both over which are overriden
	 * in SQLiteColumn.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLStatement#bindLiteral(int, java.lang.String)
	 */
	@Override
	public void bindLiteral(int paramIdx, String literalValue)
	{
		throw new UnsupportedOperationException("bindLiteral() is not supported on an AndroidSQLiteStatement.");
	}

	@Override
	public void clearAllBindings()
	{
		androidSQLiteSt.clearBindings();
	}

	@Override
	public void execute() throws DBException
	{
		androidSQLiteSt.executeInsert();
		// TODO androidSQLiteSt.executeUpdateDelete();
	}
	
}
