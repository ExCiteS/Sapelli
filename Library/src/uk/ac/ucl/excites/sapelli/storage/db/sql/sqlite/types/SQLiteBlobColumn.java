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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types;

import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;


/**
 * @author mstevens
 *
 */
public class SQLiteBlobColumn extends SQLiteColumn<byte[]>
{

	public SQLiteBlobColumn(String name, String constraint)
	{
		super(name, "BLOB", constraint, false);
	}
	
	/**
	 * @param statement
	 * @param paramIdx
	 * @param value non-null
	 */
	@Override
	protected void bind(SQLiteStatement statement, int paramIdx, byte[] value)
	{
		statement.bindBlob(paramIdx, value);
	}
	
	//TODO override toLiteralString?

}
