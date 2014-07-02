package uk.ac.ucl.excites.sapelli.shared.util;

/**
 * @author mstevens
 *
 */
public final class ExceptionHelpers
{

	private ExceptionHelpers() {}
	
	static public String getMessage(Throwable e)
	{
		if(e != null)
		{
			String msg = e.getLocalizedMessage(); 
			return msg != null && !msg.isEmpty() ? e.getLocalizedMessage() : e.toString();
		}
		else
			return null;	
	}
	
	static public String getMessageAndCause(Throwable e)
	{
		if(e != null)
			return getMessage(e) + (e.getCause() != null ? " (" + getMessage(e.getCause()) + ")" : "");
		else
			return null;
	}

}
