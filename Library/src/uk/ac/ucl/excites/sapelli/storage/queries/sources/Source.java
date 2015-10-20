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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor;

/**
 * @author mstevens
 *
 */
public abstract class Source extends Constraint
{

	// STATICS ------------------------------------------------------
	/**
	 * A Source which matches any Schema.
	 */
	static public final SourceBySet ANY = new SourceBySet(Collections.<Schema> emptySet(), SourceBySet.BY_EXCLUSION);
	
	/**
	 * A Source which does *not* match any Schema. Only for testing purposes.
	 */
	static public final SourceBySet NONE = new SourceBySet(Collections.<Schema> emptySet(), SourceBySet.BY_INCLUSION);
	
	/**
	 * Returns a {@link Source} that allows to define queries that will only return records of the given {@link Schema}
	 * 
	 * @param schema the {@link Schema} to include in the Source
	 * @return a {@link SourceBySet} that only includes the given {@link Schema}
	 */
	static public SourceBySet From(Schema schema)
	{
		if(schema == null)
			throw new NullPointerException("Provide a non-null schema");
		return From(Collections.singleton(schema));
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will only return records of the given {@link Schema}ta
	 * 
	 * @param schemata the {@link Schema}ta to include in the Source
	 * @return a {@link SourceBySet} that only includes the given {@link Schema}ta
	 */
	static public SourceBySet From(Schema... schemata)
	{
		if(schemata == null || schemata.length == 0)
			throw new NullPointerException("Provide at least 1 non-null schema");
		return From(Arrays.asList(schemata));
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will only return records of the {@link Schema}ta of the given {@link Model} 
	 * 
	 * @param model the {@link Model}ta whose {@link Schema}ta to include in the Source
	 * @return a {@link SourceBySet} that only includes the {@link Schema}ta of the given {@link Model}
	 */
	static public SourceBySet From(Model model)
	{
		if(model == null)
			throw new NullPointerException("Provide a non-null model");
		return From(model.getSchemata());
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will only return records of the given {@link Schema}ta
	 * 
	 * @param schemata the {@link Schema}ta to include in the Source
	 * @return a {@link SourceBySet} that only includes the given {@link Schema}ta
	 */
	static public SourceBySet From(Collection<Schema> schemata)
	{
		SourceBySet source = new SourceBySet(schemata != null ? schemata : Collections.<Schema> emptyList(), SourceBySet.BY_INCLUSION);
		// Check if this source makes sense:
		if(source.isNone())
			throw new NullPointerException("Provide at least 1 non-null schema");
		return source;
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will return records of any {@link Schema} *except* the given one
	 * 
	 * @param schema the {@link Schema} to include in the Source
	 * @return a {@link SourceBySet} that only includes the given {@link Schema}
	 */
	static public SourceBySet NotFrom(Schema schema)
	{
		if(schema == null)
			return ANY;
		return NotFrom(Collections.singleton(schema));
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will return records of any {@link Schema} *except* the given ones
	 * 
	 * @param schemata the {@link Schema}ta to exclude in the Source
	 * @return a {@link SourceBySet} that excludes all given {@link Schema}ta
	 */
	static public SourceBySet NotFrom(Schema... schemata)
	{
		if(schemata == null || schemata.length == 0)
			return ANY;
		return NotFrom(Arrays.asList(schemata));
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will return records of any {@link Schema} *except* the ones of the given {@link Model}
	 * 
	 * @param model the {@link Model}ta whose {@link Schema}ta to exclude in the Source
	 * @return a {@link SourceBySet} that excludes all {@link Schema}ta of the given {@link Model}
	 */
	static public SourceBySet NotFrom(Model model)
	{
		if(model == null)
			return ANY;
		return NotFrom(model.getSchemata());
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will return records of any {@link Schema} *except* the given ones
	 * 
	 * @param schemata the {@link Schema}ta to exclude in the Source
	 * @return a {@link SourceBySet} that excludes all given {@link Schema}ta
	 */
	static public SourceBySet NotFrom(Collection<Schema> schemata)
	{
		if(schemata == null || schemata.isEmpty())
			return ANY;
		return new SourceBySet(schemata, SourceBySet.BY_EXCLUSION);
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will only return records of {@link Schema}ta which have the given flags
	 * 
	 * @param flags the flags the desired {@link Schema}ta must have
	 * @return a {@link SourceByFlags} that only includes the {@link Schema}ta which have the given flags
	 */
	static public SourceByFlags With(int flags)
	{
		return new SourceByFlags(flags, SourceByFlags.BY_MATCH);
	}
	
	/**
	 * Returns a {@link Source} that allows to define queries that will return records of any {@link Schema} *except* those which have the given flags
	 * 
	 * @param flags the flags the desired {@link Schema}ta must *not* have
	 * @return a {@link SourceByFlags} that excludes all {@link Schema}ta which have the given flags
	 */
	static public SourceByFlags Without(int flags)
	{
		return new SourceByFlags(flags, SourceByFlags.BY_MISMATCH);
	}
	
	// DYNAMICS -----------------------------------------------------
	/**
	 * @return whether or not this Source matches any Schema
	 */
	public abstract boolean isAny();

	/**
	 * @return whether or not this Source does *not* match any Schema
	 */
	public abstract boolean isNone();
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#reduce()
	 */
	@Override
	public Constraint reduce()
	{
		return isAny() ? null : this;
	}
	
	/**
	 * Checks whether the Schema of the given Record is valid for this Source
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		return isValid(record.getSchema());
	}
	
	/**
	 * Checks whether the given Schema is valid for this Source
	 * 
	 * @param schema
	 * @return
	 */
	protected abstract boolean isValid(Schema schema);
	
	/**
	 * Filters a collection of schemata based on whether they are included in this Source
	 * 
	 * @param schemata
	 * @return
	 */
	public List<Schema> filterSchemata(Collection<Schema> schemata)
	{
		List<Schema> result = new ArrayList<Schema>();
		for(Schema s : schemata)
			if(isValid(s))
				result.add(s);
		return result;
	}
	
	@Override
	public void accept(ConstraintVisitor visitor) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Source#accept(ConstraintVisitor) is not implemented. Use a SourceVisitor instead.");
	}
	
	public abstract Collection<Schema> getSchemata(SourceResolver resolver);
	
}
