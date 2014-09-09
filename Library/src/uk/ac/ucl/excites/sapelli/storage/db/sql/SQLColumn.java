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

/**
 * @author mstevens
 *
 * @param <SQLT>
 * @param <SQLS>
 */
public abstract class SQLColumn<SQLT, SQLS extends SQLStatement>
{

	protected final String name;
	protected final String type;
	protected final String constraint;
	protected final boolean needsQuotes;
	
	public SQLColumn(String name, String type, String constraint, boolean needsQuotes)
	{
		this.name = name;
		this.type = type;
		this.constraint = constraint;
		this.needsQuotes = needsQuotes;
	}
	
	public String toLiteralString(SQLT value)
	{
		if(value != null)
			return (needsQuotes ?
						getQuoteChar() + value.toString().replace(getQuoteChar(), getQuoteEscape()) + getQuoteChar() :
						value.toString());
		else
			return getNullString();
	}
	
	protected abstract String getNullString();

	protected abstract String getQuoteChar();
	
	protected abstract String getQuoteEscape();
	
	/**
	 * @param statement
	 * @param paramIdx
	 * @param value non-null
	 */
	protected void bind(SQLS statement, int paramIdx, SQLT value)
	{
		statement.bindLiteral(paramIdx, toLiteralString(value));
	}
	
	/**
	 * @param statement
	 * @param paramIdx
	 */
	protected void bindNull(SQLS statement, int paramIdx)
	{
		statement.bindLiteral(paramIdx, getNullString());
	}
	
}
