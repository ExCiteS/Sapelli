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
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer.SubValueSetInitialiser;

/**
 * Importing helper class, based on a {@link ExImportHelper}, which parses
 * String representations to column values and has special behaviour for dealing with
 * {@link ListLikeColumn}s (i.e. {@link StringColumn}s and {@link ListColumn}s).
 * 
 * @see {@link ExImportHelper}
 * 
 * @author mstevens
 */
public class ImportHelper extends ExImportHelper implements SubValueSetInitialiser
{

	private String valueString;
	private Object value;
	private Exception error;
	
	/**
	 * Called by the importer to parse the given String representation and to a Column value and store it in the given valueSet.
	 * 
	 * @param column should not be null! This is not necessarily a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such, meaning it will be inspected as a whole and not broken up.
	 * @param valueString the String representation to parse, should not be {@code null} but may be empty (usually, but not always, representing a {@code null} value)
	 * @param valueSet valueSet to store the parse value in, should not be {@code null}
	 * 
	 * @see {@link #inspect(Column)}
	 */
	public final <T> void parseAndStoreValue(Column<T> column, String valueString, ValueSet<?> valueSet) throws Exception
	{
		// Ensure the column doesn't already have a value:
		if(column.isValuePresent(valueSet)) // this avoids replacing populated sub-ValueSet with empty one upon closing of outer tag
			return;
		
		// (Re)Initialise:
		this.valueString = valueString;
		this.value = null;
		this.error = null;
		
		// Visit column:
		inspect(column);
		
		// Check for errors:
		if(error != null)
			throw error;
		
		// Store value:
		column.storeObject(valueSet, value, false /*only cast, don't convert*/);
	}
	
	/* (non-Javadoc)
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	protected final <VS extends ValueSet<CS>, CS extends ColumnSet> void visit(ValueSetColumn<VS, CS> valueSetCol)
	{
		try
		{
			this.value = getSubValueSet(valueSetCol, this.valueString);
		}
		catch(Exception e)
		{
			error = e;
		}
	}
	
	/**
	 * Default implementation treats ValueSetColumn like any other (non-ListLike)Column.
	 * Subclasses may override this.
	 * 
	 * @param valueSetCol
	 * @param subValueSetString the String representation to parse, never {@code null}
	 * @return
	 * @throws Exception
	 */
	protected <VS extends ValueSet<CS>, CS extends ColumnSet> VS getSubValueSet(ValueSetColumn<VS, CS> valueSetCol, String subValueSetString) throws Exception
	{
		visit((Column<VS>) valueSetCol);
		return valueSetCol.cast(this.value);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn)
	 */
	@Override
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
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportHelper#visitVirtualColumnTargets()
	 */
	@Override
	public final boolean visitVirtualColumnTargets()
	{
		return false;
	}
	
	/**
	 * TODO explain
	 * Two reasons:
	 * 	- CSV/XML: {@link #parseAndStoreValue(Column, String, ValueSet)} only if null!
	 *  - XML: This is needed because if a column value is missing in the imported XML it means it was null (and not the default value)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer.SubValueSetInitialiser#initialise(uk.ac.ucl.excites.sapelli.storage.model.ValueSet)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> VS initialise(VS newValueSet)
	{
		if(newValueSet == null)
			return null;
		
		// Set all values to null, wiping any default values:
		newValueSet.clear();
		
		// Return vs:
		return newValueSet;
	}
	
}
