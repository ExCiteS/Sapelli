/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;


/**
 * @author mstevens
 *
 */
public interface Sender
{

	public SMSClient getSMSService();
	
	public HTTPClient getHTTPClient();
	
}
