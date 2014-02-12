package uk.ac.uk.excites.sapelli.server.db;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.ServletRequestEvent;
import javax.servlet.ServletRequestListener;

import uk.ac.uk.excites.sapelli.server.ProjectUpload;

import com.db4o.ObjectContainer;
import com.db4o.ObjectServer;
import com.db4o.cs.Db4oClientServer;
import com.db4o.cs.config.ServerConfiguration;
import com.db4o.reflect.jdk.JdkReflector;

/**
 * A listener to set up db4o database
 * 
 * @see <a
 *      href="http://docs.oracle.com/javaee/6/api/javax/servlet/ServletContextListener.html">http://docs.oracle.com/javaee/6/api/javax/servlet/ServletContextListener.html</a>
 * @see <a
 *      href="http://docs.oracle.com/javaee/6/api/javax/servlet/ServletRequestListener.html">http://docs.oracle.com/javaee/6/api/javax/servlet/ServletRequestListener..html</a>
 * 
 * @author Michalis Vitos
 * 
 */
public class Db4oServletListener implements ServletContextListener, ServletRequestListener
{
	// keys to retrieve settings values from WEB-INF/web.xml
	public static final String KEY_DB4O_FILE_NAME = "DB4O_FILENAME";
	public static final String KEY_DB4O_SERVER = "DB4O_SERVER";
	public static final String KEY_DB4O_SESSION = "DB4O_SESSION";

	/**
	 * Method called when the web application initialisation process is starting. <br>
	 * Here the db is initialised.
	 */
	@Override
	public void contextInitialized(ServletContextEvent event)
	{
		ServletContext context = event.getServletContext();
		// String filePath = context.getRealPath("WEB-INF/" + context.getInitParameter(KEY_DB4O_FILE_NAME));
		String filePath = ProjectUpload.getExCiteSFolderPath(context) + context.getInitParameter(KEY_DB4O_FILE_NAME);
	
		ServerConfiguration config = Db4oClientServer.newServerConfiguration();
		config.common().reflectWith(new JdkReflector(getClass().getClassLoader()));
	
		ObjectServer rootContainer = Db4oClientServer.openServer(config, filePath, 0);
		context.setAttribute(KEY_DB4O_SERVER, rootContainer);
		context.log("db4o startup on " + filePath);
	
		initialAuth(rootContainer);
	}

	/**
	 * Method called when the ServletContext is about to be shut down
	 */
	@Override
	public void contextDestroyed(ServletContextEvent event)
	{
		ServletContext context = event.getServletContext();
		ObjectServer rootContainer = (ObjectServer) context.getAttribute(KEY_DB4O_SERVER);
		context.removeAttribute(KEY_DB4O_SERVER);
		close(rootContainer);
		context.log("db4o shutdown");
	}

	/**
	 * Method to receive a notification that a ServletRequest is about to come into scope of the web application.
	 */
	@Override
	public void requestInitialized(ServletRequestEvent requestEvent)
	{
		ObjectServer rootContainer = (ObjectServer) requestEvent.getServletContext().getAttribute(Db4oServletListener.KEY_DB4O_SERVER);
	
		ObjectContainer session = rootContainer.openClient();
		requestEvent.getServletRequest().setAttribute(KEY_DB4O_SESSION, session);
	
		requestEvent.getServletContext().log("Started db4o session");
	}

	/**
	 * Method to receive a notification that a ServletRequest is about to go out of scope of the web application.
	 */
	@Override
	public void requestDestroyed(ServletRequestEvent requestEvent)
	{
		ObjectContainer session = (ObjectContainer) requestEvent.getServletRequest().getAttribute(KEY_DB4O_SESSION);
		requestEvent.getServletContext().log("Closed db4o session");

		close(session);
	}

	/**
	 * This returns an object container for executing queries on. WARNING: the user of this method must close the container
	 * 
	 * @param context
	 * @return
	 */
	public static ObjectContainer getObjectContainer(ServletContext context)
	{
		ObjectServer root = (ObjectServer) context.getAttribute(KEY_DB4O_SERVER);
		return root.openClient();
	}

	private void initialAuth(ObjectServer db)
	{
		/*
		 * unnecessary now the database is set up container.store(new User("admin","admin")); container.commit(); container.close();
		 */
	}

	private void close(ObjectServer db)
	{
		if(db != null)
			db.close();
	}

	private void close(ObjectContainer db)
	{
		if(db != null)
			db.close();
	}

}
