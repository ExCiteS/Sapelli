/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;


/**
 * @author mstevens
 *
 */
public interface SMSSender
{

	public boolean send(BinaryMessage binarySMS);
	
	public boolean send(TextMessage textSMS);
	
}
