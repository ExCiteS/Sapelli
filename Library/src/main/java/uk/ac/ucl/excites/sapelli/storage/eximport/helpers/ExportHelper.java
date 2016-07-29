/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

/**
 * Exporting helper class, based on a {@link ExImportHelper}, which creates
 * String representations, escaped/quoted as necessary, of column values and has special
 * behaviour for dealing with {@link ListLikeColumn}s (i.e. {@link StringColumn}s and
 * {@link ListColumn}s) and {@link VirtualColumn}s.
 * 
 * The column being inspected is a {@link VirtualColumn} the visitor is rerouted to its target column.
 * 
 * @see {@link ExImportHelper}
 * 
 * @author mstevens
 */
public abstract class ExportHelper extends ExImportHelper
{

	private Object value;
	private String valueString;
	
	/**
	 * Resets the ExportColumnValueStringProvider.
	 */
	public final void reset()
	{
		this.value = null;
		this.valueString = null;
	}
	
	/**
	 * Called by the exporter to get a single String representation for the value of the given column in the given valueSet.
	 * 
	 * @param column should not be null! This is not necessarily a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such, meaning it will be inspected as a whole and not broken up.
	 * @param valueSet the valueSet to retrieve the column value from (may be {@code null} and may contain a {@code null} value for the column, in both cases the given {@code nullString} will be returned)
	 * @param nullString value to return when either the valueSet itself is {@code null} or does not contain a non-{@code null} value for the given column
	 * @return the String representation of the value (or the given {@code nullString})
	 * 
	 * @see {@link #inspect(Column)}
	 */
	public final String getValueString(Column<?> column, ValueSet<?> valueSet, String nullString)
	{
		// Check if we have a valueSet and value:
		if(valueSet == null || !column.isValuePresent(valueSet))
			return nullString;
		
		// (Re)Initialise:
		this.value = column.retrieveValue(valueSet); // never null!
		this.valueString = null;
		
		// Inspect column:
		inspect(column);
		
		// Return String representation:
		return this.valueString != null ? this.valueString : ""; // replace null (should never happen!) by empty string
	}
	
	/**
	 * Escaping and/or quoting method.
	 * 
	 * @param valueString
	 * @param force
	 * @return
	 */
	protected abstract String escapeAndQuote(String valueString, boolean force);
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected final <T> void visit(Column<T> column)
	{
		this.valueString = escapeAndQuote(column.toString((T) value), false); // escape and quote as needed (don't force)
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	protected final <VS extends ValueSet<CS>, CS extends ColumnSet> void visit(ValueSetColumn<VS, CS> valueSetCol)
	{
		this.valueString = getSubValueSetString(valueSetCol, valueSetCol.cast(value));
	}
	
	/**
	 * Default implementation treats ValueSetColumn as any other (non-ListLike)Column.
	 * Subclasses may override this.
	 * 
	 * @param valueSetCol
	 * @param subValueSet - never {@code null}!
	 * @return
	 */
	protected  <VS extends ValueSet<CS>, CS extends ColumnSet> String getSubValueSetString(ValueSetColumn<VS, CS> valueSetCol, VS subValueSet)
	{
		visit((Column<?>) valueSetCol);
		return this.valueString;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected final <T> void visit(ListLikeColumn<T> listLikeColumn)
	{
		// don't use the columns own delimiting but do force escaping/quoting:
		this.valueString = escapeAndQuote(listLikeColumn.toString((T) value, true /*undelimited*/), true /*forced escaping & quoting*/);
	}

	/**
	 * We always export {@link VirtualColumn} values (formatted by the target column).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportHelper#visitVirtualColumnTargets()
	 */
	@Override
	protected boolean visitVirtualColumnTargets()
	{
		return true;
	}
	
}
