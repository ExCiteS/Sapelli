/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * To be used by SQLRecordStore to efficiently retrieve Schemata with specific flags.
 * 
 * @author mstevens
 */
public class BitFlagConstraint extends Constraint
{

	private final ColumnPointer<IntegerColumn> flagsColumnPointer;
	private final int flagsPattern;
	
	/**
	 * @param flagsColumn
	 * @param flagsPattern
	 */
	public BitFlagConstraint(IntegerColumn flagsColumn, int flagsPattern)
	{
		this(new ColumnPointer<IntegerColumn>(flagsColumn), flagsPattern);
	}
	
	/**
	 * @param flagsColumnPointer
	 * @param flagsPattern
	 */
	public BitFlagConstraint(ColumnPointer<IntegerColumn> flagsColumnPointer, int flagsPattern)
	{
		this.flagsColumnPointer = flagsColumnPointer;
		this.flagsPattern = flagsPattern;
	}

	/**
	 * @return the flagsColumnPointer
	 */
	public ColumnPointer<IntegerColumn> getFlagsColumnPointer()
	{
		return flagsColumnPointer;
	}

	/**
	 * @return the flagsPattern
	 */
	public int getFlagsPattern()
	{
		return flagsPattern;
	}

	/**
	 * @see StorageClient#TestSchemaFlags(int, int)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		return (((Long) flagsColumnPointer.retrieveValue(record)).intValue() & flagsPattern) == flagsPattern;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof BitFlagConstraint)
		{
			BitFlagConstraint that = (BitFlagConstraint) obj;
			return	this.flagsColumnPointer.equals(that.flagsColumnPointer) &&
					(this.flagsPattern == that.flagsPattern);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#hashCode()
	 */
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + flagsColumnPointer.hashCode();
		hash = 31 * hash + flagsPattern;
		return hash;
	}

}
