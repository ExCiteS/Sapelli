package uk.ac.ucl.excites.sapelli.transmission.util;

import uk.ac.ucl.excites.sapelli.transmission.Payload;

/**
 * @author mstevens
 *
 */
public class PayloadDecodeException extends Exception
{

	private static final long serialVersionUID = 1L;
	
	protected Payload payload;
	
	public PayloadDecodeException(Payload payload, String message, Throwable cause)
	{
		super(message, cause);
		this.payload = payload;
	}
	
	public PayloadDecodeException(Payload payload, String message)
	{
		this(payload, message, null);
	}

	/**
	 * @return the payload
	 */
	public Payload getPayload()
	{
		return payload;
	}
	
}
