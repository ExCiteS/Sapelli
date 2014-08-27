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

package uk.ac.ucl.excites.sapelli.collector.db;

import uk.ac.ucl.excites.sapelli.storage.db.sql.StoredColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 * @param <T>
 */
public abstract class SQLiteStoredColumn<T> extends StoredColumn<T>
{

	public SQLiteStoredColumn(Schema schema, Column<T> sourceColum, String name, String sqlType)
	{
		super(schema, sourceColum, name, sqlType + (sourceColum.isOptional() ? "" : " NOT NULL"));
	}

	@Override
	public String getNullString()
	{
		return "null";
	}
	
}