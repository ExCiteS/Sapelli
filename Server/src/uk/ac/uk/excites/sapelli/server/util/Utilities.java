package uk.ac.uk.excites.sapelli.server.util;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.ServletContext;

public class Utilities
{

	public Utilities()
	{
		// Do nothing
	}

	public static void printServerVersion(ServletContext context, PrintWriter printer)
	{
		printer.println("The Server is using Servlet API: " + context.getMajorVersion() + "." + context.getMinorVersion());
		printer.close();
	}

	/**
	 * Method to check that the elements have the correct type
	 * 
	 * @param clazz
	 * @param c
	 * @return
	 */
	public static <T> List<T> castList(Class<? extends T> clazz, Collection<?> c)
	{
		List<T> r = new ArrayList<T>(c.size());
		for(Object o : c)
			r.add(clazz.cast(o));
		return r;
	}

}
