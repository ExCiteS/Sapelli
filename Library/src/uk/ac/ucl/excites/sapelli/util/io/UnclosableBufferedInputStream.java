/**
 * 
 */
package uk.ac.ucl.excites.sapelli.util.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author mstevens
 *
 */
public class UnclosableBufferedInputStream extends BufferedInputStream
{
	
	private boolean allowClose = false;
	
	public UnclosableBufferedInputStream(InputStream in)
	{
		super(in);
	}

	public UnclosableBufferedInputStream(InputStream in, int size)
	{
		super(in, size);
	}
	
	public void makeClosable()
	{
		allowClose = true;
	}

	public void close() throws IOException
	{
		if(allowClose)
			forceClose();
		//else: do nothing
	}
	
	public void forceClose() throws IOException
	{
		super.close();
	}

}
