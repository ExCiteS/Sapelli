package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor;

/**
 * @author mstevens
 *
 */
public class Source extends Constraint
{

	// STATICS ------------------------------------------------------
	static private final boolean BY_INCLUSION = true;
	static private final boolean BY_EXCLUSION = !BY_INCLUSION;
	
	static public final Source ANY = new Source(Collections.<Schema> emptySet(), BY_INCLUSION);
	
	static public Source From(Schema schema)
	{
		if(schema == null)
			return ANY;
		return From(Collections.singleton(schema));
	}
	
	static public Source From(Schema... schemata)
	{
		if(schemata == null || schemata.length == 0)
			return ANY;
		return new Source(new HashSet<Schema>(Arrays.asList(schemata)), BY_INCLUSION);
	}
	
	static public Source From(Collection<Schema> schemata)
	{
		if(schemata == null || schemata.isEmpty())
			return ANY;
		return new Source(schemata, BY_INCLUSION);
	}
	
	static public Source NotFrom(Schema schema)
	{
		if(schema == null)
			return ANY;
		return NotFrom(Collections.singleton(schema));
	}
	
	static public Source NotFrom(Schema... schemata)
	{
		if(schemata == null || schemata.length == 0)
			return ANY;
		return new Source(new HashSet<Schema>(Arrays.asList(schemata)), BY_EXCLUSION);
	}
	
	static public Source NotFrom(Collection<Schema> schemata)
	{
		if(schemata == null || schemata.isEmpty())
			return ANY;
		return new Source(schemata, BY_EXCLUSION);
	}

	// DYNAMICS -----------------------------------------------------
	private final Set<Schema> schemata;
	private final boolean inclusion;
	
	/**
	 * @param schemata
	 * @param inclusion
	 */
	private Source(Collection<Schema> schemata, boolean inclusion)
	{
		this.schemata = schemata instanceof Set<?> ? (Set<Schema>) schemata : new HashSet<Schema>(schemata);
		this.inclusion = inclusion;
	}
	
	public boolean isAny()
	{
		return schemata.isEmpty();
	}

	/**
	 * @return the schemata
	 */
	public Set<Schema> getSchemata()
	{
		return schemata;
	}

	/**
	 * @return the from
	 */
	public boolean isByInclusion()
	{
		return inclusion;
	}
	
	/**
	 * @return the from
	 */
	public boolean isByExclusion()
	{
		return !inclusion;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#reduce()
	 */
	@Override
	public Constraint reduce()
	{
		return isAny() ? null : this;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public Constraint negate()
	{
		return new Source(schemata, !inclusion).reduce();
	}

	@Override
	protected boolean _isValid(Record record)
	{
		return schemata.isEmpty() || inclusion == schemata.contains(record.getSchema());
	}
	
	@Override
	public void accept(ConstraintVisitor visitor) throws UnsupportedOperationException
	{
		throw new UnsupportedOperationException("Source#accept(ConstraintVisitor) is not implemented");
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Source)
		{
			Source other = (Source) obj;
			return this.schemata.equals(other.schemata) && this.inclusion == other.inclusion;
		}
		return false;
	}

	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + schemata.hashCode();
		hash = 31 * hash + (inclusion ? 0 : 1);
		return hash;
	}
	
}
