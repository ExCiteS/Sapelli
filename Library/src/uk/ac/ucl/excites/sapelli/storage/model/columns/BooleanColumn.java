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

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 */
public class BooleanColumn extends ComparableColumn<Boolean>
{

	static private final long serialVersionUID = 2L;
	
	/**
	 * @param name
	 * @param optional
	 */
	public BooleanColumn(String name, boolean optional)
	{
		this(name, optional, null);
	}
	
	/**
	 * @param name
	 * @param optional
	 */
	public BooleanColumn(String name, boolean optional, Boolean defaultValue)
	{
		super(name, optional, defaultValue);
	}
	
	@Override
	public BooleanColumn copy()
	{
		return new BooleanColumn(name, optional);
	}
	
	/**
	 * @param value the String to parse (can be expected to be neither null nor "")
	 * @return the parsed value
	 */
	@Override
	public Boolean parse(String value)
	{
		return Boolean.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#write(java.lang.Object, uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream, boolean)
	 */
	@Override
	protected void write(Boolean value, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		bitStream.write(value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#read(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream, boolean)
	 */
	@Override
	protected Boolean read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		return bitStream.readBit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(Boolean value) throws IllegalArgumentException
	{
		//Does nothing because we allow all booleans (null check happens in super class)
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		return 1;
	}
	
	@Override
	protected int getMaximumValueSize(boolean lossless)
	{
		return 1;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return false;
	}

	@Override
	public String toString(Boolean value)
	{
		return value.toString();
	}

	@Override
	protected boolean equalRestrictions(Column<Boolean> otherColumn)
	{
		return (otherColumn instanceof BooleanColumn);		
	}

	@Override
	protected Boolean copy(Boolean value)
	{
		return Boolean.valueOf(value);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(Boolean lhs, Boolean rhs)
	{
		return lhs.compareTo(rhs);
	}

	@Override
	public Class<Boolean> getType()
	{
		return Boolean.class;
	}

}
