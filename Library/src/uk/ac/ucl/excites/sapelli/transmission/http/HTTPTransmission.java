package uk.ac.ucl.excites.sapelli.transmission.http;

import java.io.IOException;

import org.apache.commons.codec.binary.Base64;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionSender;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

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
	public HTTPTransmission(String serverURL, TransmissionClient client, Payload.Type payloadType)
	{
		super(client, payloadType);
		this.serverURL = serverURL;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 */
	public HTTPTransmission(DateTime receivedAt, TransmissionClient client, Payload.Type payloadType)
	{
		super(client, payloadType);
		setReceivedAt(receivedAt);
	}
	
	@Override
	protected void sendPayload(TransmissionSender transmissionSender) throws Exception
	{
		//HTTPClient client = sender.getHTTPClient();
		// TODO HTTP sending
	}
	
	public void setBody(String body)
	{
		this.body = body;
	}

	@Override
	protected void serialise(BitArray payloadBits) throws TransmissionCapacityExceededException
	{
		String serialisedData = Base64.encodeBase64String(payloadBits.toByteArray());
		if(serialisedData.length() > MAX_BODY_SIZE)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE + "), exceeded by " + (serialisedData.length() - MAX_BODY_SIZE) + " characters");
	}

	@Override
	protected BitArray deserialise() throws IOException
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
	
}
