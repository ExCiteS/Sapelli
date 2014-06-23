/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.util;

/**
 * @author mstevens
 *
 */
public class UnknownModelException extends Exception
{

	private static final long serialVersionUID = 2L;

	public UnknownModelException(long modelID)
	{
		super(String.format("Unknown model (ID = %d).", modelID));
	}
	
	public UnknownModelException(int schemaID, int schemaVersion)
	{
		super(String.format("Unknown model/schema (schemaID = %d; schemaVersion = %d).", schemaID, schemaVersion));
	}

}
