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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 */
public abstract class SQLTable
{

	public final String name;
	public final Schema schema;
	private final Map<ColumnPointer, ColumnMapping<?, ?>> sap2ColMap;
	private final Map<SQLColumn<?, ?>, ColumnMapping<?, ?>> sql2ColMap;
	private List<String> tableConstraints = Collections.<String> emptyList();
	
	public SQLTable(String tableName, Schema schema)
	{
		this.name = tableName;
		this.schema = schema;
		// Init collections:
		sap2ColMap = new LinkedHashMap<ColumnPointer, ColumnMapping<?,?>>();
		sql2ColMap = new LinkedHashMap<SQLColumn<?,?>, ColumnMapping<?,?>>();
	}
	
	public void addColumnMapping(ColumnMapping<?, ?> columnMapping)
	{
		sap2ColMap.put(columnMapping.sourceColumnPointer, columnMapping);
		sql2ColMap.put(columnMapping.databaseColumn, columnMapping);
	}
	
	public void setTableConstraint(List<String> tableConstraints)
	{
		this.tableConstraints = new ArrayList<String>(tableConstraints);
	}
	
	public String getCreateTableStatement()
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append("CREATE TABLE ");
		bldr.append(name);
		bldr.append(" (");
		// Columns:
		boolean first = true;
		for(SQLColumn<?, ?> sqlCol : sql2ColMap.keySet())
		{
			if(first)
				first = false;
			else
				bldr.append(", ");
			bldr.append(sqlCol.name);
			bldr.append(' ');
			bldr.append(sqlCol.type);
			if(sqlCol.constraint != null && !sqlCol.constraint.isEmpty())
			{
				bldr.append(' ');
				bldr.append(sqlCol.constraint);
			}
		}
		// Table constraints:
		for(String tConstr : tableConstraints)
		{
			bldr.append(", ");
			bldr.append(tConstr);
		}		
		bldr.append(");");
		return bldr.toString();
	}
	
	public SQLStatement getInsertStatement()
	{
		return null;
		// TODO
		//return new SQLStringStatement(generateInsertStatement(SQLStringStatement.PARAM_PLACEHOLDER));
	}
	
	protected String generateInsertStatement(char paramPlaceholder)
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append("INSERT INTO ");
		bldr.append(name);
		bldr.append(" (");
		// Columns:
		boolean first = true;
		for(SQLColumn<?, ?> sqlCol : sql2ColMap.keySet())
		{
			if(first)
				first = false;
			else
				bldr.append(", ");
			bldr.append(sqlCol.name);
		}
		bldr.append(") VALUES (");
		first = true;
		for(int i = 0; i < sql2ColMap.size(); i++)
		{
			if(i > 0)
				bldr.append(", ");
			bldr.append(paramPlaceholder);
		}
		bldr.append(");");
		return bldr.toString();
	}
	
	public SQLColumn<?, ?> getDatabaseColumn(ColumnPointer columnPointer)
	{
		ColumnMapping<?, ?> colMap = sap2ColMap.get(columnPointer);
		if(colMap != null)
			return colMap.databaseColumn;
		return null;
	}
	
	public Schema getSchema()
	{
		return schema;
	}

}
