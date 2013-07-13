/**
 * 
 */
package uk.ac.ucl.excites.util;

/**
 * @author mstevens
 *
 */
public final class ThreadUtils
{

	private ThreadUtils() {}
	
	static public void sleep(int ms)
	{
		try
		{
			Thread.sleep(ms);
		}
		catch(InterruptedException ignore) {}
	}
	
}
