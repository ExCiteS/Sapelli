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

import java.io.IOException;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionSendingException;


/**
 * @author mstevens
 *
 */
public abstract class SMSTransmission<M extends Message> extends Transmission<SMSCorrespondent>
{
	
	// STATIC -------------------------------------------------------
	
	public static final int MAX_RESEND_REQUESTS = 5;
	
	/**
	 * Returned values:<br/>
	 * 	1th request: wait  00:12:00; total elapsed time: 12 mins<br/>
	 * 	2th request: wait  01:00:00; total elapsed time: 1 hours 12 mins<br/>
	 * 	3th request: wait  05:00:00; total elapsed time: 6 hours 12 mins<br/>
	 * 	4th request: wait  25:00:00; total elapsed time: 1 days  7 hours 12 mins<br/>
	 * 	5th request: wait 125:00:00; total elapsed time: 6 days 12 hours 12 mins (= almost a week)<br/>
	 * 	6th-... request: NO MORE RESENDS!
	 * 
	 * @param requestNumber number of the request (counting from 1; i.e. it is the amount of reqs already sent + 1)
	 * @return the number ms to wait between the before the request may be sent (after the reception of the last part or the sending of the previous request), or -1 if no more request should be sent for this transmission
	 */
	public static int GetResendDelayMS(int requestNumber)
	{
		if(requestNumber > MAX_RESEND_REQUESTS)
			return -1;
		return (int) (Math.pow(5, requestNumber - 2) * 60 * 60 * 1000);
	}
	
	// DYNAMIC ------------------------------------------------------
	protected final SortedSet<M> parts = new TreeSet<M>();
	
	private TimeStamp deliveredAt;
	private int numberOfSentResendRequests;
	private TimeStamp lastResendRequestSentAt;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param client
	 * @param receiver
	 * @param payload
	 */
	public SMSTransmission(TransmissionClient client, SMSCorrespondent receiver, Payload payload)
	{
		super(client, receiver, payload);
	}
		
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param firstReceivedPart
	 */
	public SMSTransmission(TransmissionClient client, M firstReceivedPart)
	{
		super(client, firstReceivedPart.getSender(), firstReceivedPart.getSendingSideTransmissionID(), firstReceivedPart.getPayloadHash()); // pass on the remoteID & payload hash
		receivePart(firstReceivedPart);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param correspondent
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param parts - list of {@link Message}s
	 * @param numberOfSentResendRequests
	 * @param lastResendReqSentAt - may be null
	 */
	protected SMSTransmission(TransmissionClient client, SMSCorrespondent correspondent, int localID, Integer remoteID, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, int numberOfSentResendRequests, TimeStamp lastResendReqSentAt)
	{
		super(client, correspondent, localID, remoteID, payloadHash, sentAt, receivedAt);
		this.numberOfSentResendRequests = numberOfSentResendRequests;
		this.lastResendRequestSentAt = lastResendReqSentAt;
		// add parts by calling receivePart (not by passing them through this constructor)
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Transmission#getSentCallback()
	 */
	@Override
	public SentCallback getSentCallback()
	{
		return new SentCallback(); // this is an SMSTransmission.SentCallback, not a plain Transmission.SentCallback
	}

	/**
	 * To be called on receiving side, or upon database retrieval on both the sending and the receiving side.
	 * 
	 * @param msg
	 */
	public void receivePart(M msg)
	{
		if(!parts.isEmpty())
		{	// Each message that's received after the first one must have a matching remote transmission id, payload hash, sender & total # of parts:
			String error = null;
			if(remoteID != msg.getSendingSideTransmissionID())
				error = "remote ID mismatch";
			else if(payloadHash != msg.getPayloadHash())
				error = "Payload hash mismatch";
			else if(!correspondent.equals(msg.getSender()))
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
			// the reception time of the most recently received part becomes the receivedAt time of the transmission as a whole:
			setReceivedAt(lastReceivedAt);
		}
	}
	
	public SortedSet<M> getParts()
	{
		return parts;
	}
	
	/**
	 * Get part (Message) by part number
	 * 
	 * @param partNumber a value from [1, totalParts]
	 * @return
	 */
	public M getPart(int partNumber)
	{
		for(M part : parts)
			if(part.getPartNumber() == partNumber)
				return part;
		return null;
	}
	
	/**
	 * @param partNumber a value from [1, totalParts]
	 * @return
	 */
	public boolean hasPart(int partNumber)
	{
		return getPart(partNumber) != null;
	}
	
	/**
	 * To be called on receiving side
	 * 
	 * @return the part that was most recently received
	 */
	public M getLastReceivedPart()
	{
		if(parts.isEmpty())
			return null;
		M mostRecent = null;
		for(M part : parts)
			if(mostRecent == null || part.receivedAt.isAfter(mostRecent.receivedAt))
				mostRecent = part;
		return mostRecent;
	}
	
	/**
	 * @return the current number of (received) parts
	 */
	public int getCurrentNumberOfParts()
	{
		return parts.size();
	}
	
	/**
	 * @return the total number of (expected) parts
	 */
	public int getTotalNumberOfParts()
	{
		if(parts.isEmpty())
			throw new IllegalStateException("Total number of parts not known yet");	// can only happen on sending side prior to calling of wrap(), or when database retrieval was not correctly performed
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
			throw new IllegalStateException("Total number of parts not known yet");	// can only happen on sending side prior to calling of wrap(), or when database retrieval was not correctly performed
		return (parts.first().getTotalParts() == parts.size());
	}
	
	@Override
	protected void doSend(TransmissionController controller) throws TransmissionSendingException
	{
		if(parts.isEmpty())
			throw new TransmissionSendingException("No messages to send.");
		
		// Send unsent messages one by one:
		for(Message m : parts)
			if(!m.isSent())
				m.send(controller.getSMSClient());
	}
	
	@Override
	public void resend(TransmissionController controller) throws IOException, TransmissionCapacityExceededException, TransmissionSendingException
	{
		// Clear sentAt of messages (otherwise they can't be resent):
		for(Message m : parts)
			m.setSentAt(null);
		
		// Do re-send:
		super.resend(controller); // will clear sentAt time of transmission and then call send()
	}
	
	/**
	 * Resends one or more specific parts
	 * 
	 * @param controller
	 * @param partNumbers
	 * @throws TransmissionSendingException
	 */
	public void resend(TransmissionController controller, List<Integer> partNumbers) throws TransmissionSendingException
	{
		for(Message m : parts)
			if(partNumbers.contains(Integer.valueOf(m.getPartNumber())))
			{
				// Clear sentAt of message (otherwise we cannot re-send it):
				m.setSentAt(null);
				// Re-send:
				m.send(controller.getSMSClient());
			}
	}
	
	public TimeStamp getDeliveredAt()
	{
		return deliveredAt;
	}

	/**
	 * @param deliveredAt the deliveredAt to set
	 */
	protected void setDeliveredAt(TimeStamp deliveredAt)
	{
		this.deliveredAt = deliveredAt;
	}

	/**
	 * @return the numberOfSentResendRequests
	 */
	public int getNumberOfSentResendRequests()
	{
		return numberOfSentResendRequests;
	}
	
	/**
	 * @param numberOfSentResendRequests the numberOfSentResendRequests to set
	 */
	public void setNumberOfSentResendRequests(int numberOfSentResendRequests)
	{
		this.numberOfSentResendRequests = numberOfSentResendRequests;
	}
	
	/**
	 * @return the lastResendRequestSentAt
	 */
	public TimeStamp getLastResendRequestSentAt()
	{
		return lastResendRequestSentAt;
	}
	
	/**
	 * @param lastResendRequestSentAt the lastResendRequestSentAt to set
	 */
	public void setLastResendRequestSentAt(TimeStamp lastResendRequestSentAt)
	{
		this.lastResendRequestSentAt = lastResendRequestSentAt;
	}

	/**
	 * @return time at which to send next resend request, or null if no (more) request should be sent
	 * 
	 * @see {@link #GetResendDelayMS(int)}
	 */
	public TimeStamp getNextResendRequestSendingTime()
	{
		if(	isComplete() ||										// transmission is already complete 
			numberOfSentResendRequests >= MAX_RESEND_REQUESTS)	// we are giving up on this transmission (the missing parts may still alive but we won't send additional resend requests)
			return null;
		
		// Time of last part reception or last resent request sending (whichever happened last):
		TimeStamp prev = TimeStamp.Latest(getLastReceivedPart().getReceivedAt(), lastResendRequestSentAt);
		
		return prev.shift(GetResendDelayMS(numberOfSentResendRequests + 1));
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public class SentCallback extends Transmission<SMSCorrespondent>.SentCallback
	{
		
		public void onSent(Message msg)
		{
			onSent(msg, TimeStamp.now());
		}
		
		public void onSent(Message msg, TimeStamp sentAt)
		{
			// Mark msg as sent:
			msg.setSentAt(sentAt);

			// Check if whole transmission has been sent and get sentAt time of last msg to be sent:
			TimeStamp tSentAt = getLatest(msg, new TimeStampDelegate()
			{
				@Override
				public TimeStamp getTimeStamp(Message m)
				{
					return m.getSentAt();
				}
			});
			
			// Mark transmission as sent, or only store it (to store sentAt time of msg):
			if(tSentAt != null)
			{	// all parts have been sent, use the sending time of the most recently sent part as the sentAt time for the transmission as a whole:
				onSent(tSentAt); // also calls store();
			}
			else
				store();
		}
		
		public void onDelivered(Message msg)
		{
			onDelivered(msg, TimeStamp.now());
		}
		
		public void onDelivered(Message msg, TimeStamp deliveredAt)
		{
			// Mark msg as sent:
			msg.setDeliveredAt(deliveredAt);
			
			// Check if whole transmission has been delivered and get deliveredAt time of last msg to be delivered:
			TimeStamp tDeliveredAt = getLatest(msg, new TimeStampDelegate()
			{
				@Override
				public TimeStamp getTimeStamp(Message m)
				{
					return m.getDeliveredAt();
				}
			});
			
			// Mark transmission as sent, or only store it (to store sentAt time of msg):
			if(tDeliveredAt != null)
				setDeliveredAt(tDeliveredAt);
			
			store(); // !!!
		}
		
		private TimeStamp getLatest(Message msg, TimeStampDelegate delegate)
		{
			// Just in case:
			if(SMSTransmission.this != msg.getTransmission())
				throw new IllegalStateException("Transmission mismatch");
			
			// Check if this was the last part to be sent:
			TimeStamp latest = null;
			for(Message m : parts)
			{
				TimeStamp ts = delegate.getTimeStamp(m);
				if(ts == null) // this means Message m hasn't been sent/delivered yet
				{
					latest = null; // indicates the transmission hasn't been fully sent/delivered
					break;
				}
				if(latest == null || latest.isBefore(ts))
					latest = ts;
			}
			return latest;
		}
		
	}
	
	/**
	 * @author mstevens
	 */
	private interface TimeStampDelegate
	{
		
		public TimeStamp getTimeStamp(Message m);
		
	}
	
}
