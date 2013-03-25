/**
 * 
 */
package uk.ac.ucl.excites.transmission.compression;

/**
 * @author mstevens
 *
 */
public class CompressorException extends Exception
{
	
	private static final long serialVersionUID = 1L;
	
	public CompressorException(Exception cause)
	{
		super(cause);
	}

	public CompressorException(String msg)
	{
		super(msg);
	}
	
	public CompressorException(String msg, Exception cause)
	{
		super(msg, cause);
	}

}
