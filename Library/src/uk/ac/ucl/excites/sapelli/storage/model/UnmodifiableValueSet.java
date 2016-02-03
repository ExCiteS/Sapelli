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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;

/**
 * @author mstevens
 *
 */
public class UnmodifiableValueSet<CS extends ColumnSet> extends ValueSet<CS>
{

	private static final long serialVersionUID = 2L;

	/**
	 * Creates an initialised UnmodifiableValueSet.
	 * 
	 * @param columnSet
	 * @param values to initialise the ValueSet with, number of values must match number of (real) columns in the ColumnSet and each value must be valid for the corresponding Column
	 */
	public UnmodifiableValueSet(CS columnSet, Object... values)
	{
		super(columnSet, values);
	}

	/**
	 * Creates an initialised UnmodifiableValueSet.
	 * 
	 * @param columnSet
	 * @param serialisedValues String to initialise ValueSet with (should not contain values of virtual columns, i.e. the String must be as produced by {@link #serialise()})
	 * @throws Exception 
	 */
	public UnmodifiableValueSet(CS columnSet, String serialisedValues) throws Exception
	{
		super(columnSet, serialisedValues);
	}

	/**
	 * Creates an initialised UnmodifiableValueSet.
	 * 
	 * @param columnSet
	 * @param serialisedValues byte array to initialise ValueSet with (should not contain values of virtual columns and may or may not be {@code lossless}ly encoded, i.e. the array must be as produced by {@code #toBytes(lossless)})
	 * @param lossless whether the given byte array is a (guaranteed) lossless ({@code true}), or a (possibly) lossy ({@code false}) representation of the values
	 * @throws NullPointerException when schema is null
	 * @throws IOException when reading serialisedValues fails
	 */
	public UnmodifiableValueSet(CS columnSet, byte[] serialisedValues, boolean lossless) throws NullPointerException, IOException
	{
		super(columnSet, serialisedValues, lossless);
	}
	
	/**
	 * Copy constructor. Creates an initialised UnmodifiableValueSet with the values of another ValueSet.
	 *  
	 * @param another
	 */
	public UnmodifiableValueSet(ValueSet<CS> another)
	{
		super(another);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#setValue(uk.ac.ucl.excites.sapelli.storage.model.Column, java.lang.Object)
	 */
	@Override
	protected void setValue(Column<?> column, Object value) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#parse(java.lang.String)
	 */
	@Override
	public ValueSet<CS> parse(String serialisedRecord) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#parse(java.lang.String, boolean, java.util.Set)
	 */
	@Override
	public ValueSet<CS> parse(String serialisedRecord, boolean includeVirtual, Set<? extends Column<?>> skipColumns) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#fromBytes(byte[], boolean)
	 */
	@Override
	public ValueSet<CS> fromBytes(byte[] bytes, boolean allLossless) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#readFromBitStream(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream, boolean)
	 */
	@Override
	public void readFromBitStream(BitInputStream bitStream, boolean allLossless) throws IOException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#readFromBitStream(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream, boolean, java.util.Set, boolean)
	 */
	@Override
	public void readFromBitStream(BitInputStream bitStream, boolean includeVirtual, Set<? extends Column<?>> skipColumns, boolean allLossless) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ValueSet#readColumnsFromBitStream(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream, java.util.List, boolean)
	 */
	@Override
	public void readColumnsFromBitStream(BitInputStream bitStream, List<? extends Column<?>> columns, boolean lossless) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Cannot set or change values in an " + UnmodifiableValueSet.class.getSimpleName());
	}

}
