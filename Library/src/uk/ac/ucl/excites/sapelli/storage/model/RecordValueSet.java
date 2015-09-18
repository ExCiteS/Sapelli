package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.util.IncompletePrimaryKeyException;

public abstract class RecordValueSet<CS extends ColumnSet> extends ValueSet<CS>
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param columnSet
	 * @param serialisedValues
	 * @throws NullPointerException
	 * @throws IOException
	 */
	public RecordValueSet(CS columnSet, byte[] serialisedValues) throws NullPointerException, IOException
	{
		super(columnSet, serialisedValues);
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
	 * @param serialisedValues
	 * @throws Exception
	 */
	public RecordValueSet(CS columnSet, String serialisedValues) throws Exception
	{
		super(columnSet, serialisedValues);
	}

	/**
	 * @param columnSet
	 */
	public RecordValueSet(CS columnSet)
	{
		super(columnSet);
	}

	/**
	 * @param another
	 */
	public RecordValueSet(ValueSet<CS> another)
	{
		super(another);
	}

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * @return a query that looks for this record
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 */
	public abstract SingleRecordQuery getRecordQuery() throws IncompletePrimaryKeyException;

	/**
	 * Shared method of {@link Record} and {@link RecordReference}.
	 * 
	 * @return a Constraint
	 * @throws IncompletePrimaryKeyException when the columns that are part of the primary key have not all been assigned a value
	 */
	public abstract Constraint getRecordQueryConstraint() throws IncompletePrimaryKeyException;

}