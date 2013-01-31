/**
 * 
 */
package uk.ac.ucl.excites.storage.db;

/**
 * @author mstevens
 *
 */
public final class DataAccess
{

	static private DataAccess INSTANCE = null;
	
	static public DataAccess getInstance(String dbFilePath)
	{
		if(INSTANCE == null)
			INSTANCE = new DataAccess(dbFilePath);
		return INSTANCE;
	}
	
	//private DB4O ...
	
	private DataAccess(String dbFilePath)
	{
		//TODO
	}
	
	//TODO access methods
	
}
