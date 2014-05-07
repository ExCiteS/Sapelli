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
		return e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.toString();
	}
	
	static public String getMessageAndCause(Throwable e)
	{
		return getMessage(e) + e.getCause() != null ? " (" + getMessage(e.getCause()) + ")" : "";
	}

}
