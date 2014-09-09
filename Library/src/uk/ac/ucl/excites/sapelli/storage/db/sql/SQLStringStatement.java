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

package uk.ac.ucl.excites.sapelli.storage.db.sql;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;

/**
 * @author mstevens
 *
 */
public class SQLStringStatement extends SQLStatement
{

	static public final char PARAM_PLACEHOLDER = '?';
	
	private SQLRecordStore store;
	private String sql;
	
	/**
	 * 
	 */
	public SQLStringStatement(SQLRecordStore store, String sql)
	{
		this.store = store;
		this.sql = sql;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql2.SQLStatement#bindLiteral(int, java.lang.String)
	 */
	@Override
	public void bindLiteral(int paramIdx, String literalValue)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void execute() throws DBException
	{
		store.executeSQL(sql);
	}

	@Override
	public void clearAllBindings()
	{
		// TODO Auto-generated method stub
		
	}

}
