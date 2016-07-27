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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidColumnException;

/**
 * Special column to keep track of the lossless/lossy-ness of {@link ValueSet}s/{@link Record}s.
 * It is automatically added to {@link Schema}ta which have the {@link StorageClient#SCHEMA_FLAG_TRACK_LOSSLESSNESS} flag.
 * 
 * @author mstevens
 */
public final class LosslessFlagColumn extends BooleanColumn
{

	// STATIC -------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static public final String NAME = "LosslessFlag";

	static public final Boolean VALUE_LOSSLESS = Boolean.TRUE;
	static public final Boolean VALUE_LOSSY = Boolean.valueOf(!VALUE_LOSSLESS.booleanValue());
	
	static public final LosslessFlagColumn INSTANCE = new LosslessFlagColumn();
	
	// DYNAMIC ------------------------------------------------------
	private LosslessFlagColumn()
	{
		super(	NAME,
				false,				// never optional
				VALUE_LOSSLESS);	// default value: we always start out losslessly
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn#copy()
	 */
	@Override
	protected BooleanColumn createCopy()
	{
		return INSTANCE;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return true; // !!!
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn#write(java.lang.Boolean, uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream, boolean)
	 */
	@Override
	protected void write(Boolean value, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		super.write(value && lossless, bitStream, lossless); // !!! the value remains lossless until it is written lossyly, after that it remains lossy
	}
	
	/**
	 * Checks if the given {@link ValueSet}, which is assumed to be of a {@link ColumnSet} containing the {@link LosslessFlagColumn}
	 * (like a {@link Schema} with flag {@link StorageClient#SCHEMA_FLAG_TRACK_LOSSLESSNESS}), is in a lossless state. 
	 *
	 * @param valueSet the {@link ValueSet}
	 * @return whether or not the valueSet is in a lossless state
	 * @throws NullPointerException if the given {@link ValueSet} is {@code null}
	 * @throws InvalidColumnException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> boolean isLossless(VS valueSet) throws NullPointerException, InvalidColumnException
	{
		return retrieveValue(valueSet) == VALUE_LOSSLESS;
	}
	
	/**
	 * Checks if the given {@link ValueSet}, which is assumed to be of a {@link ColumnSet} containing the {@link LosslessFlagColumn}
	 * (like a {@link Schema} with flag {@link StorageClient#SCHEMA_FLAG_TRACK_LOSSLESSNESS}), is in a lossy state. 
	 *
	 * @param valueSet the {@link ValueSet}
	 * @return whether or not the valueSet is in a lossy state
	 * @throws NullPointerException if the given {@link ValueSet} is {@code null}
	 * @throws InvalidColumnException when this column is not part of the valueSet's {@link ColumnSet}, nor compatible with a column by the same name that is
	 */
	public <VS extends ValueSet<CS>, CS extends ColumnSet> boolean isLossy(VS valueSet) throws NullPointerException, InvalidColumnException
	{
		return retrieveValue(valueSet) == VALUE_LOSSY;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#addVirtualVersion(uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn)
	 */
	@Override
	protected <VT> void addVirtualVersion(VirtualColumn<VT, Boolean> virtualVersion) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("LosslessFlagColumn cannot have VirtualColumns!");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#getTypeString()
	 */
	@Override
	public String getTypeString()
	{
		return NAME;
	}
	
	@Override
	protected boolean equalRestrictions(Column<Boolean> otherColumn)
	{
		return (otherColumn instanceof LosslessFlagColumn);		
	}
	
}
