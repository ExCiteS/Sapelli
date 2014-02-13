/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.util;

/**
 * @author mstevens
 *
 */
public class DuplicateException extends Exception
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * @param detailMessage
	 */
	public DuplicateException(String detailMessage)
	{
		super(detailMessage);
	}

}
