/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.transmission.modes.sms;

import java.util.SortedSet;
import java.util.TreeSet;

import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission<M extends Message> extends Transmission
{
	
	protected SMSAgent receiver;
	protected SMSAgent sender;
	
	protected final SortedSet<M> parts = new TreeSet<M>();
	
	private TimeStamp deliveredAt;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param receiver
	 * @param client
	 * @param payloadType
	 */
	public SMSTransmission(TransmissionClient client, SMSAgent receiver, Payload payload)
	{
		super(client, payload);
		this.receiver = receiver;
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param firstReceivedPart
	 */
	public SMSTransmission(TransmissionClient client, M firstReceivedPart)
	{
		super(client, firstReceivedPart.getSendingSideTransmissionID(), firstReceivedPart.getPayloadHash()); // pass on the remoteID & payload hash
		receivePart(firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param sender - may be null on sending side
	 * @param receiver - may be null on receiving side
	 * @param parts - list of {@link Message}s
	 */
	protected SMSTransmission(TransmissionClient client, int localID, Integer remoteID, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, SMSAgent sender, SMSAgent receiver)
	{
		super(client, localID, remoteID, payloadHash, sentAt, receivedAt);
		this.sender = sender;
		this.receiver = receiver;
		// add parts by calling receivePart (not by passing them through this constructor)
	}
	
	/**
	 * To be called on receiving side.
	 * 
	 * @param msg
	 */
	public void receivePart(M msg)
	{
		if(parts.isEmpty())
			// If this is the first part: register the sender
			this.sender = msg.getSender();
		else
		{	// Each following received message must have a matching remote transmission id, payload hash, sender & total # of parts:
			String error = null;
			if(remoteID != msg.getSendingSideTransmissionID())
				error = "remote ID mismatch";
			else if(payloadHash != msg.getPayloadHash())
				error = "Payload hash mismatch";
			else if(!sender.equals(msg.getSender()))
				error = "sender mismatch";
			else if(parts.first().getTotalParts() != msg.getTotalParts())
				error = "different number of parts";
			if(error != null)
				throw new IllegalArgumentException("This message does not belong to the same transmission (" + error + ")");
		}
		// Check for duplicates:
		if(parts.contains(msg))
			return; // discard duplicate
		// Add the part:
		parts.add(msg);
		msg.setTransmission(this);
		// If all parts are received, set overall reception time: 
		if(isComplete())
		{
			TimeStamp lastReceivedAt = null;
			for(Message m : parts)
				if(lastReceivedAt == null || lastReceivedAt.isBefore(m.getReceivedAt()))
					lastReceivedAt = m.getReceivedAt();
			setReceivedAt(lastReceivedAt);
		}
	}
	
	public SortedSet<M> getParts()
	{
		return parts;
	}
	
	public Message getPart(int i)
	{
		for(Message part : parts)
			if(part.getPartNumber() == i)
				return part;
		return null;
	}
	
	public boolean hasPart(int i)
	{
		return getPart(i) != null;
	}
	
	public int getCurrentNumberOfParts()
	{
		return parts.size();
	}
	
	public int getTotalNumberOfParts()
	{
		return parts.first().getTotalParts(); // Do not use parts.size() because that is not correct for incomplete transmissions on the receiving side
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @return whether all parts have been received
	 */
	@Override
	public boolean isComplete()
	{
		if(parts.isEmpty())
			return false;
		return (parts.first().getTotalParts() == parts.size());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#doSend(uk.ac.ucl.excites.sapelli.transmission.TransmissionSender)
	 */
	@Override
	protected void doSend(TransmissionController transmissionSender)
	{
		if(parts.isEmpty())
			throw new IllegalStateException("No messages to send.");
		
		//Send unsent messages one by one:
		for(Message m : parts)
			if(!m.isSent())
				m.send(transmissionSender.getSMSService());
	}

	/**
	 * Resends a specific part (single message)
	 * 
	 * @param transmissionSender
	 * @param partNumber
	 */
	public void resend(TransmissionController transmissionSender, int partNumber)
	{
		int i = 1; // partNumbers start from 1!
		for(Message m : parts)
			if(i == partNumber)
			{
				m.send(transmissionSender.getSMSService());
				return;
			}
			else
				i++;
	}
	
	/**
	 * Part has been sent
	 * 
	 * @param smsMessage
	 */
	protected void partSent(Message smsMessage)
	{
		TimeStamp lastSentAt = null;
		for(Message m : parts)
		{
			if(!m.isSent())
				return;
			if(lastSentAt == null || lastSentAt.isBefore(m.getSentAt()))
				lastSentAt = m.getSentAt();
		}
		setSentAt(lastSentAt);
	}
	
	/**
	 * Part has been delivered to relay
	 * 
	 * @param smsMessage
	 */
	protected void partDelivered(Message smsMessage)
	{
		TimeStamp lastDeliveredAt = null;
		for(Message m : parts)
		{
			if(!m.isReceived())
				return;
			if(lastDeliveredAt == null || lastDeliveredAt.isBefore(m.getDeliveredAt()))
				lastDeliveredAt = m.getDeliveredAt();
		}
		deliveredAt = lastDeliveredAt;		
	}
	
	public SMSAgent getReceiver()
	{
		return receiver;
	}
	
	public boolean isReceiverSet()
	{
		return receiver != null;
	}
	
	public SMSAgent getSender()
	{
		return sender;
	}
	
	public boolean isSenderSet()
	{
		return sender != null;
	}
	
	public TimeStamp getDeliveredAt()
	{
		return deliveredAt;
	}
	
}
