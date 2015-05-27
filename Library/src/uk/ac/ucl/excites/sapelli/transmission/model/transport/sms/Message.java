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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms;

import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionSendingException;

/**
 * Abstract class representing an SMS message which is one part of an {@link SMSTransmission}
 * 
 * @author mstevens
 */
public abstract class Message implements Comparable<Message>
{
	
	// STATIC -------------------------------------------------------
	static public interface Handler
	{
		
		public void handle(BinaryMessage binMsg);
		
		public void handle(TextMessage txtMsg);
		
	}
	
	// DYNAMIC ------------------------------------------------------
	protected SMSTransmission<?> transmission;
	
	/**
	 * Only used on sending side.
	 */
	protected TimeStamp sentAt;
	
	/**
	 * Only used on sending side.
	 */
	protected TimeStamp deliveredAt;
	
	/**
	 * Only used on receiving side. 
	 */
	protected TimeStamp receivedAt;
	
	/**
	 * Only used on receiving side. 
	 */
	protected SMSCorrespondent sender;
	
	/**
	 * Only used on receiving side. 
	 */
	protected Integer sendingSideTransmissionID;
	
	/**
	 * Only used on receiving side. 
	 */
	protected Integer payloadHash;
	
	/**
	 * a value from [1, totalParts]
	 */
	protected int partNumber;
	
	protected int totalParts;
	
	/**
	 * To be called on sending side.
	 * 
	 * @param receiver
	 * @param transmission
	 * @param partNumber a value from [1, totalParts]
	 * @param totalParts
	 */
	public Message(SMSTransmission<?> transmission, int partNumber, int totalParts)
	{
		// Some checks:
		if(transmission == null)
			throw new NullPointerException("Transmission cannot be null when creating new Message on sending side or upon database retrieval.");
		if(partNumber < 1 || totalParts < 1 || partNumber > totalParts)
			throw new IllegalArgumentException("Invalid part number (" + partNumber + ") of total number of parts (" + totalParts + ").");
		
		// Transmission:
		this.transmission = transmission;
		
		// Part numbers:
		this.partNumber  = partNumber;
		this.totalParts = totalParts;
	}
	
	/**
	 * To be called on receiving side
	 * 
	 * @param sender
	 */
	public Message(SMSCorrespondent sender, TimeStamp receivedAt)
	{
		this.sender = sender;
		this.receivedAt = receivedAt;
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param transmission
	 * @param partNumber
	 * @param totalParts
	 * @param sentAt - may be null
	 * @param deliveredAt - may be null
	 * @param receivedAt - may be null
	 */
	public Message(SMSTransmission<?> transmission, int partNumber, int totalParts, TimeStamp sentAt, TimeStamp deliveredAt, TimeStamp receivedAt)
	{
		this(transmission, partNumber, totalParts);
		this.sentAt = sentAt;
		this.deliveredAt = deliveredAt;
		this.receivedAt = receivedAt;
		this.payloadHash = transmission.getPayloadHash();
		if(transmission.received) //Alternative: if(!isSent() && isReceived())
		{	// if on receiving side:
			this.sender = transmission.getCorrespondent();
			this.sendingSideTransmissionID = transmission.getRemoteID();
		}
		else
		{	// if on sending side:
			this.sendingSideTransmissionID = transmission.getLocalID();
		}
	}
	
	public void send(SMSClient smsClient) throws TransmissionSendingException
	{
		// Set values required for message header:
		try
		{
			this.sendingSideTransmissionID = transmission.getLocalID();
			this.payloadHash = transmission.getPayloadHash();
		}
		catch(Exception e)
		{
			throw new TransmissionSendingException("Cannot send Message until header values are set.", e);
		}
		
		// Send the message:
		doSend(smsClient);
	}
	
	protected abstract void doSend(SMSClient smsClient) throws TransmissionSendingException;

	/**
	 * @return the partNumber
	 */
	public int getPartNumber()
	{
		return partNumber;
	}

	/**
	 * @return the totalParts
	 */
	public int getTotalParts()
	{
		return totalParts;
	}

	public boolean isSent()
	{
		return (sentAt != null);
	}
	
	public TimeStamp getSentAt()
	{
		return sentAt;
	}
	
	protected void setSentAt(TimeStamp sentAt)
	{
		this.sentAt = sentAt;
	}
	
	public boolean isReceived()
	{
		return (receivedAt != null);
	}
	
	public TimeStamp getReceivedAt()
	{
		return receivedAt;
	}
	
	public boolean isDelivered()
	{
		return (deliveredAt != null);
	}
	
	public TimeStamp getDeliveredAt()
	{
		return deliveredAt;
	}

	protected void setDeliveredAt(TimeStamp deliveredAt)
	{
		this.deliveredAt = deliveredAt;
	}
	
	/**
	 * @return the SMSCorrespondent which this message originates from (on receiving side), or null (on sending side)
	 */
	public SMSCorrespondent getSender()
	{
		return sender;
	}
	
	/**
	 * @return the sendingSideTransmissionID
	 * @throws IllegalArgumentException when no sendingSideTransmissionID has been set
	 */
	public int getSendingSideTransmissionID() throws IllegalArgumentException
	{
		if(sendingSideTransmissionID == null)
			throw new IllegalStateException("sendingSideTransmissionID has not been set yet");
		return sendingSideTransmissionID;
	}
	
	/**
	 * @return
	 * @throws IllegalArgumentException when no payloadHash has been set
	 */
	public int getPayloadHash() throws IllegalArgumentException
	{
		if(payloadHash == null)
			throw new IllegalStateException("Payload hash has not been set yet");
		return payloadHash;
	}

	/**
	 * @return the transmission this Message belongs to, or null if no Transmission has been set yet (can only happen on receiving side)
	 */
	public SMSTransmission<?> getTransmission()
	{
		return transmission;
	}
	
	/**
	 * Only used on receiving side.
	 * Should only ever be called from {@link SMSTransmission#receivePart(Message)}.
	 * 
	 * @param transmission
	 */
	protected void setTransmission(SMSTransmission<?> transmission)
	{
		if(this.transmission != null && this.transmission != transmission)
			throw new IllegalStateException("Cannot change transmission.");
		this.transmission = transmission;
	}
	
	/**
	 * @return
	 */
	public abstract Transmission.Type getTransmissionType();
		
	@Override
	public int compareTo(Message another)
	{
		return this.getPartNumber() - another.getPartNumber();
	}
	
	protected abstract int getBodyHashCode();
	
	/**
	 * hashCode() method
	 * Deliberately ignores transmission (but not sendingSideTransmissionID), sentAt, deliveredAt & receivedAt.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + Objects.hashCode(sender);
		hash = 31 * hash + Objects.hashCode(sendingSideTransmissionID);
		hash = 31 * hash + Objects.hashCode(payloadHash);
		hash = 31 * hash + partNumber;
		hash = 31 * hash + totalParts;
		hash = 31 * hash + getBodyHashCode();
		return hash;
	}
	
	/**
	 * equals() method
	 * Deliberately ignores transmission (but not sendingSideTransmissionID), sentAt, deliveredAt & receivedAt.
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof Message)
		{
			Message that = (Message) obj;
			return	Objects.equals(this.sender, that.sender) &&
					Objects.equals(this.sendingSideTransmissionID, that.sendingSideTransmissionID) &&
					Objects.equals(this.payloadHash, that.payloadHash) &&
					this.partNumber == that.partNumber &&
					this.totalParts == that.totalParts &&
					equalBody(that);			
		}
		return false;
	}
	
	protected abstract boolean equalBody(Message another);
	
	/**
	 * @param handler
	 */
	public abstract void handle(Handler handler);
	
}
