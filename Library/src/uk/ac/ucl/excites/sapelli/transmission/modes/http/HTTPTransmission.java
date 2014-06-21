package uk.ac.ucl.excites.sapelli.transmission.modes.http;

import java.io.IOException;

import org.apache.commons.codec.binary.Base64;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.Sender;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 */
public class HTTPTransmission extends Transmission
{
	
	public static final int MAX_BODY_SIZE = 4096; //TODO determine a good value
	
	private String serverURL;
	private String body = null;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param serverURL
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
	 */
	public HTTPTransmission(TransmissionClient client, int payloadHash, String body, DateTime receivedAt)
	{
		super(client, payloadHash);
		this.body = body;
		setReceivedAt(receivedAt);
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
		String serialisedData = Base64.encodeBase64String(payloadBits.toByteArray());
		if(serialisedData.length() > MAX_BODY_SIZE)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE + "), exceeded by " + (serialisedData.length() - MAX_BODY_SIZE) + " characters");
		this.body = serialisedData;
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		if(body == null || body.isEmpty())
			throw new IllegalStateException("Transmission body is not set or empty.");
		return BitArray.FromBytes(Base64.decodeBase64(body));
	}

	@Override
	public int getMaxPayloadBits()
	{
		return MAX_BODY_SIZE * Byte.SIZE;
	}

	@Override
	public boolean isComplete()
	{
		return body != null && !body.isEmpty();
	}
	
}
