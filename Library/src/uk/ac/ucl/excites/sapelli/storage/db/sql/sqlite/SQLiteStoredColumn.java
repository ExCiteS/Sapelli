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

import uk.ac.ucl.excites.sapelli.storage.db.sql.StoredColumn;
import uk.ac.ucl.excites.sapelli.storage.model.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;

/**
 * @author mstevens
 *
 * @param <T>
 */
public abstract class SQLiteStoredColumn<T> extends StoredColumn<T>
{
	
	static private <T> String GetColSpec(Schema schema, Column<T> sourceColum, String name, String sqlType)
	{
		StringBuilder bldr = new StringBuilder(sqlType);
		
		// Primary key:
		PrimaryKey pk = schema.getPrimaryKey();
		if(pk != null /* just in case*/ && !pk.isMultiColumn() && pk.containsColumn(sourceColum, false))
		{
			bldr.append(" PRIMARY KEY");
			// ASC/DESC?
			// conflict-clause?
			if(pk instanceof AutoIncrementingPrimaryKey)
				bldr.append(" AUTOINCREMENT");
		}
		
		// Regular single-column, unique index:
		Index idx = schema.getIndex(sourceColum);
		if(idx != null && !idx.isMultiColumn() && idx.isUnique())
		{
			bldr.append(" UNIQUE");
			// conflict-clause?
		}
			
		// Optionality:
		if(sourceColum.isOptional())
			bldr.append(" NOT NULL");
		
		// TODO Default value?
		
		// TODO foreign-key-clause?
		if(sourceColum instanceof ForeignKeyColumn)
		{
			// ...
		}
		
		return bldr.toString();
	}

	public SQLiteStoredColumn(Schema schema, Column<T> sourceColum, String name, String sqlType)
	{
		super(schema, sourceColum, name, GetColSpec(schema, sourceColum, name, sqlType));
	}

	@Override
	public String getNullString()
	{
		return "null";
	}
	
}