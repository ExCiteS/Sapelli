/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import uk.ac.ucl.excites.transmission.http.HTTPClient;
import uk.ac.ucl.excites.transmission.sms.SMSService;


/**
 * @author mstevens
 *
 */
public interface TransmissionSender
{

	public SMSService getSMSService();
	
	public HTTPClient getHTTPClient();
	
}
