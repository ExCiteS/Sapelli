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

package uk.ac.ucl.excites.sapelli.storage.eximport.helpers;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * Importing helper class, based on a {@link ExImportColumnValueHelper}, which parses
 * String representations to column values and has special behaviour for dealing with
 * {@link ListLikeColumn}s (i.e. {@link StringColumn}s and {@link ListColumn}s).
 * 
 * @see {@link ExImportColumnValueHelper}
 * 
 * @author mstevens
 */
public class ImportColumnValueParser extends ExImportColumnValueHelper
{

	private String valueString;
	private Object value;
	private Exception error;
	
	/**
	 * Called by the importer to parse the given String representation and to a Column value and store it in the given valueSet.
	 * 
	 * @param column should not be null! This is not necessarily a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such, meaning it will be inspected as a whole and not broken up.
	 * @param valueString the String representation to parse, should not be {@code null}
	 * @param valueSet valueSet to store the parse value in, should not be {@code null}
	 * 
	 * @see {@link #inspect(Column)}
	 */
	@SuppressWarnings("unchecked")
	public final <T> void parseAndStoreValue(Column<T> column, String valueString, ValueSet<?> valueSet) throws Exception
	{
		// (Re)Initialise:
		this.valueString = valueString; // string to parse
		this.value = null;
		this.error = null;
		
		// Visit column:
		inspect(column);
		
		// Check for errors:
		if(error != null)
			throw error;
		
		// Store value:
		column.storeValue(valueSet, (T) value);
	}
	
	/**
	 * Used for all {@link Column}s except {@link ListLikeColumn}s.
	 * 
	 * @param column
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@Override
	protected final <T> void visit(Column<T> column)
	{
		try
		{
			this.value = column.stringToValue(valueString);
			/*// Equivalent to:
			this.value = (valueString != null && !valueString.isEmpty() ? column.parse(valueString) : null);*/
		}
		catch(Exception e)
		{
			error = e;
		}
	}
	
	/**
	 * Used for all {@link ListLikeColumn}s.
	 * 
	 * @param listLikeColumn
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn)
	 */
	protected final <T> void visit(ListLikeColumn<T> listLikeColumn)
	{
		try
		{
			this.value = (valueString != null ? listLikeColumn.parse(valueString, true /*undelimited*/) : null);
		}
		catch(Exception e)
		{
			error = e;
		}
	}

	/**
	 * Always ignore {@link VirtualColumn}s during importing.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visitVirtualColumnTargets()
	 */
	@Override
	public final boolean visitVirtualColumnTargets()
	{
		return false;
	}
	
}
