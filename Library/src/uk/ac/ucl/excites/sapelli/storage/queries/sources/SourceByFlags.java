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

package uk.ac.ucl.excites.sapelli.storage.queries.sources;

import java.util.Collection;

import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;

/**
 * @author mstevens
 *
 */
public class SourceByFlags extends Source
{

	// STATICS ------------------------------------------------------
	static /*package*/ final boolean BY_MATCH = true;
	static /*package*/ final boolean BY_MISMATCH = !BY_MATCH;
	
	// DYNAMICS -----------------------------------------------------
	private final int flags;
	private final boolean match;
	
	/**
	 * @param flags
	 * @param match
	 */
	/*package*/ SourceByFlags(int flags, boolean match)
	{
		this.flags = flags;
		this.match = match;
	}

	/**
	 * @return the flags
	 */
	public int getFlags()
	{
		return flags;
	}

	@Override
	public boolean isAny()
	{
		return flags == 0 && isByMatch();
	}

	@Override
	public boolean isNone()
	{
		return flags == 0 && isByMismatch();
	}
	
	public boolean isByMatch()
	{
		return match;
	}
	
	public boolean isByMismatch()
	{
		return !match;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public Constraint negate()
	{
		return new SourceByFlags(flags, !match).reduce();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.Source#isValid(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
	protected boolean isValid(Schema schema)
	{
		return match == schema.hasFlags(flags);
	}
	
	@Override
	public Collection<Schema> getSchemata(SourceResolver resolver)
	{
		return resolver.resolve(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof SourceByFlags)
		{
			SourceByFlags that = (SourceByFlags) obj;
			return	this.flags == that.flags &&
					this.match == that.match;
		}
		return false;
	}

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + getClass().getName().hashCode();
		hash = 31 * hash + flags;
		hash = 31 * hash + (match ? 0 : 1);
		return hash;
	}
	
}
