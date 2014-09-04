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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 */
public class TableSpec
{

	private final String tableName;
	private final Schema schema;
	private final ColumnSpec<?>[] columnSpecs; // in order
	private final Map<ColumnPointer, ColumnSpec<?>> columnMapping;
	private final String[] tableConstraints;
	
	public TableSpec(Schema schema, List<ColumnSpec<?>> storedColumns, List<String> tableConstraints)
	{
		this(schema.getName(), schema, storedColumns, tableConstraints);
	}
	
	public TableSpec(String tableName, Schema schema, List<ColumnSpec<?>> columnSpecs, List<String> tableConstraints)
	{
		this.tableName = tableName;
		this.schema = schema;
		this.columnSpecs = columnSpecs.toArray(new ColumnSpec<?>[columnSpecs.size()]);
		this.tableConstraints = tableConstraints.toArray(new String[tableConstraints.size()]);
		// Populate mapping:
		this.columnMapping = new HashMap<ColumnPointer, ColumnSpec<?>>();
		for(ColumnSpec<?> colSpec : columnSpecs)
			columnMapping.put(colSpec.sourceColumnPointer, colSpec);
	}
	
	public String getCreateTableStatement()
	{
		StringBuilder bldr = new StringBuilder();
		bldr.append("CREATE TABLE ");
		bldr.append(tableName);
		bldr.append(" (");
		// Columns:
		for(ColumnSpec<?> c : columnSpecs)
		{
			if(c != columnSpecs[0])
				bldr.append(", ");
			bldr.append(c.name);
			bldr.append(' ');
			bldr.append(c.typeAndConstraint);
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
	
	public String getInsertStatement(Record record)
	{			
		StringBuilder bldr = new StringBuilder();
		bldr.append("INSERT INTO ");
		bldr.append(tableName);
		bldr.append(" (");
		for(ColumnSpec<?> c : columnSpecs)
		{
			if(c != columnSpecs[0])
				bldr.append(", ");
			bldr.append(c.name);
		}
		bldr.append(") VALUES (");
		for(ColumnSpec<?> c : columnSpecs)
		{
			if(c != columnSpecs[0])
				bldr.append(", ");
			bldr.append(c.getStorableValueString(record));
		}
		bldr.append(");");
		return bldr.toString();
	}
	
	public ColumnSpec<?> getStoredColumn(ColumnPointer columnPointer)
	{
		return columnMapping.get(columnPointer);
	}
	
	public Schema getSchema()
	{
		return schema;
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <T>
	 */
	static public abstract class ColumnSpec<T>
	{
		
		final ColumnPointer sourceColumnPointer;
		final String name;
		final String typeAndConstraint;
		
		public ColumnSpec(Schema schema, Column<T> sourceColum, String name, String type, String constraint)
		{
			this.sourceColumnPointer = new ColumnPointer(schema, sourceColum);
			this.name = name;
			this.typeAndConstraint = type + (constraint != null && !constraint.isEmpty() ? " " + constraint : "");
		}
		
		@SuppressWarnings("unchecked")
		public String getStorableValueString(Record record)
		{
			return toStorableString((T) sourceColumnPointer.retrieveValue(record));
		}
		
		@SuppressWarnings("unchecked")
		public String getStorableValueString(Object value)
		{
			return toStorableString((T) value);
		}
		
		public String toStorableString(T value)
		{
			if(value == null)
				return getNullString();
			else
				return toStorableValueString(value);
		}
		
		public abstract String getNullString();
		
		/**
		 * @param value (guaranteed non-null)
		 * @return
		 */
		public abstract String toStorableValueString(T value);
		
	}

}
