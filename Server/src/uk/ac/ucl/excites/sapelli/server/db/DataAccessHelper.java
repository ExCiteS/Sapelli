package uk.ac.ucl.excites.sapelli.server.db;

import javax.servlet.http.HttpServletRequest;

import uk.ac.ucl.excites.collector.project.db.DataAccess;

import com.db4o.ObjectContainer;

public class DataAccessHelper
{

	public DataAccessHelper()
	{
		// Do nothing
	}

	public static DataAccess getInstance(HttpServletRequest request)
	{
		// Get the session container
		ObjectContainer db = (ObjectContainer) request.getAttribute(Db4oServletListener.KEY_DB4O_SESSION);
		return new DataAccess(db);
	}

}
