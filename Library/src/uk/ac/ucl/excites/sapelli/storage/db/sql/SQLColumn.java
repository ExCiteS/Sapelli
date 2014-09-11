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

import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 * @param <SQLType>
 */
public abstract class SQLColumn<SQLType, SapType>
{

	public final String name;
	protected final String type;
	protected final String constraint;
	protected final ColumnPointer sourceColumnPointer;
	protected final TypeMapping<SQLType, SapType> mapping;
	
	/**
	 * @param name
	 * @param type
	 * @param constraint
	 * @param sourceColumnPointer
	 * @param sourceMapping
	 */
	@SuppressWarnings("unchecked")
	public SQLColumn(String name, String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
	{
		this.name = name;
		this.type = type;
		this.constraint = constraint;
		this.sourceColumnPointer = sourceColumnPointer;
		this.mapping = mapping != null ? mapping : (TypeMapping<SQLType, SapType>) TypeMapping.<SQLType> Transparent();
	}
	
	/**
	 * @param value
	 * @param quotedIfNeeded
	 * @return
	 */
	public String sapToLiteral(SapType value, boolean quotedIfNeeded)
	{
		return sqlToLiteral(mapping.toSQLType(value), quotedIfNeeded);
	}
	
	/**
	 * @param value
	 * @param quotedIfNeeded
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public String sapObjectToLiteral(Object value, boolean quotedIfNeeded)
	{
		return sqlToLiteral(mapping.toSQLType((SapType) value), quotedIfNeeded);
	}
	
	/**
	 * @param record
	 * @param quotedIfNeeded
	 * @return
	 */
	public String retrieveAsLiteral(Record record, boolean quotedIfNeeded)
	{
		return sqlToLiteral(retrieve(record), quotedIfNeeded);
	}
	
	/**
	 * @param record
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public SQLType retrieve(Record record)
	{
		return mapping.toSQLType((SapType) sourceColumnPointer.retrieveValue(record));
	}
	
	/**
	 * @param record
	 * @param value
	 */
	public void store(Record record, SQLType value)
	{
		sourceColumnPointer.getColumn().storeObject(record, mapping.toSapelliType(value));
	}
	
	/**
	 * To be overridden when quotes are needed (e.g. on Strings)
	 * 
	 * @return
	 */
	protected boolean needsQuotedLiterals()
	{
		return false;
	}
	
	protected String sqlToLiteral(SQLType value, boolean quotedIfNeeded)
	{
		if(value != null)
			return (quotedIfNeeded && needsQuotedLiterals() ?
						getQuoteChar() + value.toString().replace(getQuoteChar(), getQuoteEscape()) + getQuoteChar() :
						value.toString());
		else
			return getNullString();
	}
	
	protected abstract String getNullString();

	protected abstract String getQuoteChar();
	
	protected abstract String getQuoteEscape();
	
}
