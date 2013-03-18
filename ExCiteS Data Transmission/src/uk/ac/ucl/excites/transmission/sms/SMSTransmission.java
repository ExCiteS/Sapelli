/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;


/**
 * @author mstevens
 *
 */
public class SMSTransmission extends Transmission
{
	
	//Statics
	public static final int SMS_MODE_BINARY = 0;
	public static final int SMS_MODE_TEXT = 1;
	
	//Dynamics
	private int mode = SMS_MODE_BINARY;
	//private BitArray data;
	
	private int id;
	private boolean full = false;
	private SMSReceiver receiver;
	private int smsMode;
	private SMSSender sender;

	private ArrayList<Message> messages;

	public SMSTransmission(Schema schema, Integer id, SMSReceiver receiver, SMSSender sender)
	{
		super(schema);
		this.id = id;
		
		this.receiver = receiver;
		this.smsMode = receiver.getSmsMode();
		this.sender = sender;
	}
	
	@Override
	public boolean addRecord(Record record)
	{
		
		
		return true;
	}



	public boolean isFull()
	{
		return full;
	}

	public int getID()
	{
		return id;
	}

//	public void addObservation(Observation observation)
//	{
//		BitArray observationBits = observation.toBitArray(false);
//		
//		int headerSize = 12 * 8; //TODO move to statics
//		
//		BitArray newContent = new BitArray(headerSize + (content == null ? 0 : content.size()) + observationBits.size());
//		
//		//TODO Set header (may not yet?)
//		//TODO Copy old content
//		
//		content = newContent;
//		//TODO add observationBits to content
//		
//		//TODO Compress
//		
//		//TODO Encrypt
//		
//		//TODO SERIALISE/SPLIT
//		
//		//TODO check is everything fits
//		
//		//TODO Accept observation or reject it, if reject: mark as full
//		observations.add(observation);
//		observation.setTransaction(this);
//	}

	@Override
	public void send()
	{
		/* create and send sms messages */

		
		
	}

	public void resend(int part)
	{

	}
	
	public void partSent(Message smsMessage)
	{
		
	}

	public void partReceived(Message smsMessage)
	{
		
	}
	
}
