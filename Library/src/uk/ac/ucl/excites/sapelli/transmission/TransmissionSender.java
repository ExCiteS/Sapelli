/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import uk.ac.ucl.excites.sapelli.transmission.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSService;


/**
 * @author mstevens
 *
 */
public interface TransmissionSender
{

	public SMSService getSMSService();
	
	public HTTPClient getHTTPClient();
	
}
