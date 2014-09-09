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

import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLTable;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class SQLiteTable extends SQLTable
{

	private SQLiteRecordStore store;
	private SQLiteStatement insertStatement;

	public SQLiteTable(String tableName, Schema schema, SQLiteRecordStore store)
	{
		super(tableName, schema);
		this.store = store;
	}
	
	@Override
	public SQLiteStatement getInsertStatement()
	{
		if(insertStatement == null)
			insertStatement = store.createStatement(generateInsertStatement(store.getParamPlaceholder()));
		else
			insertStatement.clearAllBindings();
		return insertStatement;
	}

}
