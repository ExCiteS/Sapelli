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

/**
 * @author mstevens
 *
 * @param <SQLT>
 * @param <SQLS>
 */
public abstract class SQLiteColumn<SQLT> extends SQLColumn<SQLT, SQLiteStatement>
{

	public SQLiteColumn(String name, String type, String constraint, boolean needsQuotes)
	{
		super(name, type, constraint, needsQuotes);
	}
	
	/**
	 * Make abstract to avoid bindLiteral() is ever called on SQLiteStatements
	 *  
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLColumn#bind(uk.ac.ucl.excites.sapelli.storage.db.sql2.SQLStatement, int, java.lang.Object)
	 */
	@Override
	protected abstract void bind(SQLiteStatement statement, int paramIdx, SQLT value);
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql2.SQLColumn#bindNull(uk.ac.ucl.excites.sapelli.storage.db.sql2.SQLStatement, int)
	 */
	@Override
	protected void bindNull(SQLiteStatement statement, int paramIdx)
	{
		statement.bindNull(paramIdx);
	}

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

}
