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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.util.IncompletePrimaryKeyException;

/**
 * Abstract superclass for {@link Record} and {@link RecordReference}.
 * 
 * @author mstevens
 *
 * @param <CS>
 */
public abstract class RecordValueSet<CS extends ColumnSet> extends ValueSet<CS>
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param columnSet
	 */
	public RecordValueSet(CS columnSet)
	{
		super(columnSet);
	}

	/**
	 * @param columnSet
	 * @param values
	 */
	public RecordValueSet(CS columnSet, Object... values)
	{
		super(columnSet, values);
	}
	
	/**
	 * @param columnSet
	 * @param serialisedValues String to initialise ValueSet with (should not contain values of virtual columns, i.e. the String must be as produced by {@link #serialise()})
	 * @throws Exception
	 */
	public RecordValueSet(CS columnSet, String serialisedValues) throws Exception
	{
		super(columnSet, serialisedValues);
	}
	
	/**
	 * @param columnSet
	 * @param serialisedValues byte array to initialise ValueSet with (should not contain values of virtual columns and may or may not be {@code lossless}ly encoded, i.e. the array must be as produced by {@code #toBytes(lossless)})
	 * @param lossless whether the given byte array is a (guaranteed) lossless ({@code true}), or a (possibly) lossy ({@code false}) representation of the values
	 * @throws NullPointerException
	 * @throws IOException
	 */
	public RecordValueSet(CS columnSet, byte[] serialisedValues, boolean lossless) throws NullPointerException, IOException
	{
		super(columnSet, serialisedValues, lossless);
	}

	/**
	 * @param another
	 */
	public RecordValueSet(ValueSet<CS> another)
	{
		super(another);
	}
	
	/**
	 * If this is a {@link Record} the method returns the record's {@link Schema},
	 * if this is a {@link RecordReference} the method returns the "referenced" {@link Schema}.
	 * 
	 * @return a {@link Schema} instance
	 */
	protected abstract Schema getSchema();
	
	/**
	 * @return whether or not (all parts of) the primary key have a non-null value
	 */
	public final boolean isReferenceable()
	{
		try
		{
			return getReference().isFilled(true);
		}
		catch(IncompletePrimaryKeyException ipke)
		{
			return false;
		}
	}
	
	/**
	 * If this is a {@link Record} the method returns a {@link RecordReference} pointing to this {@link Record},
	 * if this is a {@link RecordReference} the method returns the object itself.
	 * 
	 * @return a {@link RecordReference} instance
	 * @throws IncompletePrimaryKeyException if (part of) the primary key column(s) lacks a value
	 */
	public abstract RecordReference getReference() throws IncompletePrimaryKeyException;

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * Returns a {@link SingleRecordQuery} which can be used to find this (if this is a {@link Record}),
	 * or the referenced (if this is a {@link RecordReference}), record in a RecordStore or Collection.
	 * 
	 * @return a query that looks for this Record(Reference)
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key (and thus covered by a recordReference) have not all been assigned a value
	 */
	public final SingleRecordQuery getRecordQuery() throws IncompletePrimaryKeyException
	{
		return new FirstRecordQuery(Source.From(getSchema()), Order.UNDEFINED, getRecordQueryConstraint());
	}

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * @return a Constraint that matches this Record(Reference)
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 */
	public final Constraint getRecordQueryConstraint() throws IncompletePrimaryKeyException
	{
		return getRecordQueryConstraint(false); // don't allow blanks!
	}
	
	/**
	 * @param allowBlanks if {@code true} no IncompletePrimaryKeyException is thrown
	 * @return
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key (and thus covered by a recordReference) have not all been assigned a value, and {@code allowBlanks} is {@code false}
	 */
	/*package*/ abstract Constraint getRecordQueryConstraint(boolean allowBlanks) throws IncompletePrimaryKeyException;

}