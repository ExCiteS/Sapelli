/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.util.Date;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 * 
 */
public abstract class Transmission
{

	protected Date sentAt;
	protected Date receivedAt;
	protected Date confirmationSentAt;
	protected Date confirmationReceivedAt;
	
	protected Schema schema;

	
	public abstract boolean addRecord(Record record);
	
	
}
