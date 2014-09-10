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

import uk.ac.ucl.excites.sapelli.shared.db.DBException;

public abstract class SQLiteStatement
{
	
	/**
	 * @param paramIdx
	 * @param column
	 * @param value
	 */
	public <SQLT> void bind(int paramIdx, SQLiteColumn<SQLT> column, SQLT value)
	{
		if(value != null)
			column.bind(this, paramIdx, value);
		else
			column.bindNull(this, paramIdx);
	}
	
	public abstract void bindBlob(int paramIdx, byte[] value);
	
	public abstract void bindLong(int paramIdx, Long value);
	
	public abstract void bindDouble(int paramIdx, Double value);
	
	public abstract void bindString(int paramIdx, String value);
	
	public abstract void bindNull(int paramIdx);
	
	/**
	 * TODO remove ?
	 * 
	 * @param paramIdx
	 * @param literalValue
	 */
	public abstract void bindLiteral(int paramIdx, String literalValue);
	
	public abstract void execute() throws DBException;
	
	public abstract void clearAllBindings();

}
