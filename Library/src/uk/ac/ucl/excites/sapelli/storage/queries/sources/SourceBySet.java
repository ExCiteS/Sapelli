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
import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;

/**
 * @author mstevens
 *
 */
public class SourceBySet extends Source
{

	// STATICS ------------------------------------------------------
	static /*package*/ final boolean BY_INCLUSION = true;
	static /*package*/ final boolean BY_EXCLUSION = !BY_INCLUSION;
	
	// DYNAMICS -----------------------------------------------------
	private final Set<Schema> schemata;
	private final boolean inclusion;
	
	/**
	 * @param schemata
	 * @param inclusion
	 */
	/*package*/ SourceBySet(Collection<Schema> schemata, boolean inclusion)
	{
		this.schemata = new HashSet<Schema>();
		CollectionUtils.addAllIgnoreNull(this.schemata, schemata);
		this.inclusion = inclusion;
	}
	
	@Override
	public boolean isAny()
	{
		return schemata.isEmpty() && isByExclusion();
	}
	
	@Override
	public boolean isNone()
	{
		return schemata.isEmpty() && isByInclusion();
	}

	/**
	 * @return the schemata
	 */
	public Set<Schema> getSchemata()
	{
		return schemata;
	}

	public boolean isByInclusion()
	{
		return inclusion;
	}
	
	public boolean isByExclusion()
	{
		return !inclusion;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public Constraint negate()
	{
		return new SourceBySet(schemata, !inclusion).reduce();
	}
	
	/**
	 * The call to {@link Set#contains(Object)} uses the expensive {@link Schema#equals(Object)} method to compare pairs of schemata, but the search itself is O(log2(n)).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.sources.Source#isValid(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
	protected boolean isValid(Schema schema)
	{
		return inclusion == schemata.contains(schema);
	}
	
	/**
	 * Alternative isValid() method which allows by-passing the expensive schema comparison and use a cheaper (but less) secure implementation.
	 * When {@code fullSchemaCompare} is false a cheaper schema pair comparison will be used, but the search itself is O(n).
	 * 
	 * @param record
	 * @param fullSchemaCompare
	 * @return
	 */
	public boolean isValid(Record record, boolean fullSchemaCompare)
	{
		return isValid(record.getSchema(), fullSchemaCompare);
	}
	
	/**
	 * Alternative isValid() method which allows by-passing the expensive schema comparison and use a cheaper (but less) secure implementation.
	 * When {@code fullSchemaCompare} is false a cheaper schema pair comparison will be used, but the search itself is O(n).
	 * 
	 * @param schema
	 * @param fullSchemaCompare
	 * @return
	 */
	public boolean isValid(Schema schema, boolean fullSchemaCompare)
	{
		if(schema == null)
			return false;
		else if(fullSchemaCompare)
			return isValid(schema); // uses the "expensive" isValid() above
		else
		{	// Uses cheaper schema pair comparison, but the search is O(n)
			boolean contains = false;
			for(Schema sourceSchema : schemata)
				if(schema.equals(sourceSchema, false, false, false))
				{
					contains = true;
					break;
				}
			return inclusion == contains;
		}
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
		if(obj instanceof SourceBySet)
		{
			SourceBySet other = (SourceBySet) obj;
			return this.schemata.equals(other.schemata) && this.inclusion == other.inclusion;
		}
		return false;
	}

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + getClass().getName().hashCode();
		hash = 31 * hash + schemata.hashCode();
		hash = 31 * hash + (inclusion ? 0 : 1);
		return hash;
	}
	
}
