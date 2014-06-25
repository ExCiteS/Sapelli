package uk.ac.ucl.excites.sapelli.transmission.modes.http;

import java.io.IOException;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.Sender;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 * http://stackoverflow.com/questions/3049626/sending-binary-data-via-post-on-android
 * http://stackoverflow.com/questions/20508788/do-i-need-content-type-application-octet-stream-for-file-download
 * http://stackoverflow.com/questions/4047731/send-imagejpg-via-httppost-from-android-to-servletwebserver
 * Use mimi type: "application/octet-stream"
 */
public class HTTPTransmission extends Transmission
{
	
	public static final int MAX_BODY_SIZE = 4096; // bytes (TODO determine a good value)
	
	private String serverURL;
	private byte[] body;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param client
	 * @param serverURL
	 * @param payload
	 */
	public HTTPTransmission(TransmissionClient client, String serverURL, Payload payload)
	{
		super(client, payload);
		this.serverURL = serverURL;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param payloadHash
	 * @param body
	 * @param receivedAt
	 */
	public HTTPTransmission(TransmissionClient client, int payloadHash, byte[] body, DateTime receivedAt)
	{
		super(client, payloadHash);
		this.body = body;
		setReceivedAt(receivedAt);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param localID
	 * @param serverURL
	 * @param payloadHash
	 * @param body
	 */
	public HTTPTransmission(TransmissionClient client, int localID, String serverURL, int payloadHash, byte[] body) 
	{
		super(client, payloadHash);
		this.localID = localID;
		this.serverURL = serverURL;
		this.body = body;
	}
	
	@Override
	protected void doSend(Sender transmissionSender)
	{
		//HTTPClient client = sender.getHTTPClient();
		// TODO HTTP sending
	}

	@Override
	protected void wrap(BitArray payloadBits) throws TransmissionCapacityExceededException
	{
		byte[] payloadBytes = payloadBits.toByteArray();
 		//String serialisedData = Base64.encodeBase64String(payloadBytes);
		if(payloadBytes.length > MAX_BODY_SIZE)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE + "), exceeded by " + (payloadBytes.length - MAX_BODY_SIZE) + " bytes");
		this.body = payloadBytes;
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		if(body == null)
			throw new IllegalStateException("Transmission body has not been set.");
		return BitArray.FromBytes(body); //Base64.decodeBase64(body)
	}
	
	@Override
	public boolean isComplete()
	{
		return body != null;
	}
	
	public byte[] getBody()
	{
		return body;
	}
	
	/**
	 * @return the serverURL
	 */
	public String getServerURL()
	{
		return serverURL;
	}

	@Override
	public int getMaxPayloadBits()
	{
		return MAX_BODY_SIZE * Byte.SIZE;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#canWrapCanIncreaseSize()
	 */
	@Override
	public boolean canWrapIncreaseSize()
	{
		return false;
	}

	@Override
	public Type getType()
	{
		return Type.HTTP;
	}
	
}
