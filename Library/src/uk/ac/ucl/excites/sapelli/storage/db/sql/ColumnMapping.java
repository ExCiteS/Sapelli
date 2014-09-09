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

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 * @param <SapT, SQLT>
 */
public abstract class ColumnMapping<SapT, SQLT>
{
	
	final ColumnPointer sourceColumnPointer;
	final SQLColumn<SQLT, ?> databaseColumn;
	
	public ColumnMapping(Schema schema, Column<SapT> sapelliColum, SQLColumn<SQLT, ?> databaseColumn)
	{
		this.sourceColumnPointer = new ColumnPointer(schema, sapelliColum);
		this.databaseColumn = databaseColumn;
	}

	/**
	 * @param value
	 * @return
	 */
	protected abstract SQLT toDatabaseType(SapT value);
	
	/**
	 * @param value
	 * @return
	 */
	protected abstract SapT toSapelliType(SQLT value);
	
}