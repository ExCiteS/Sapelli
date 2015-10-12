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
 * Exporting helper class, based on a {@link ExImportColumnValueHelper}, which creates
 * String representations, escaped/quoted as necessary, of column values and has special
 * behaviour for dealing with {@link ListLikeColumn}s (i.e. {@link StringColumn}s and
 * {@link ListColumn}s) and {@link VirtualColumn}s.
 * 
 * The column being inspected is a {@link VirtualColumn} the visitor is rerouted to its target column.
 * 
 * @see {@link ExImportColumnValueHelper}
 * 
 * @author mstevens
 */
public abstract class ExportColumnValueStringProvider extends ExImportColumnValueHelper
{

	private Object value;
	private String valueString;
	
	/**
	 * Called by the exporter to get a single String representation for the value of the given column in the given valueSet.
	 * 
	 * @param column should not be null! This is not necessarily a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such, meaning it will be inspected as a whole and not broken up.
	 * @param valueSet should not be {@code null} and contain a non-n{@code null} value for the given column
	 * @return the String representation of the value
	 * 
	 * @see {@link #inspect(Column)}
	 */
	public final String toString(Column<?> column, ValueSet<?> valueSet)
	{
		// (Re)Initialise:
		this.value = column.retrieveValue(valueSet);
		this.valueString = null;
		
		// Inspect column:
		inspect(column);
		
		// Return String representation:
		return this.valueString;
	}
	
	/**
	 * Escaping and/or quoting method.
	 * 
	 * @param valueString
	 * @param force
	 * @return
	 */
	protected abstract String escapeAndQuote(String valueString, boolean force);

	/**
	 * Used for all {@link Column}s except {@link ListLikeColumn}s.
	 * 
	 * @param column
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visit(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected final <T> void visit(Column<T> column)
	{
		this.valueString = escapeAndQuote(column.toString((T) value), false); // escape and quote as needed (don't force)
	}
	
	/**
	 * Used for all {@link ListLikeColumn}s. 
	 * 
	 * @param listLikeColumn
	 * 
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
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExImportColumnValueHelper#visitVirtualColumnTargets()
	 */
	@Override
	protected boolean visitVirtualColumnTargets()
	{
		return true;
	}
	
}
