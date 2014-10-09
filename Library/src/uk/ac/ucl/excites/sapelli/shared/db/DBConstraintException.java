/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.db;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class DBConstraintException extends DBException
{

	private static final long serialVersionUID = 2L;

	/**
	 * @param detailMessage
	 * @param cause
	 */
	public DBConstraintException(String detailMessage, Throwable cause)
	{
		super(detailMessage, cause);
	}

	/**
	 * @param detailMessage
	 * @param cause
	 * @param records
	 */
	public DBConstraintException(String detailMessage, Throwable cause, Record... records)
	{
		super(detailMessage, cause, records);
	}

	/**
	 * @param detailMessage
	 */
	public DBConstraintException(String detailMessage)
	{
		super(detailMessage);
	}

	/**
	 * @param detailMessage
	 * @param records
	 */
	public DBConstraintException(String detailMessage, Record... records)
	{
		super(detailMessage, records);
	}

	/**
	 * @param cause
	 */
	public DBConstraintException(Throwable cause)
	{
		super(cause);
	}

	/**
	 * @param cause
	 * @param records
	 */
	public DBConstraintException(Throwable cause, Record... records)
	{
		super(cause, records);
	}

}
