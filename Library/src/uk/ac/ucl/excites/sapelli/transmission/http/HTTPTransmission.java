package uk.ac.ucl.excites.sapelli.transmission.http;

import java.io.IOException;

import org.apache.commons.codec.binary.Base64;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.transmission.BinaryTransmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionSender;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

public class HTTPTransmission extends BinaryTransmission
{
	
	public static final int MAX_BODY_SIZE = 4096; //TODO determine a good value
	
	private String serverURL;
	private String body = null;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param serverURL
	 */
	public HTTPTransmission(String serverURL)
	{
		super();
		this.serverURL = serverURL;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 */
	public HTTPTransmission(DateTime receivedAt, TransmissionClient client)
	{
		super(client);
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
	protected void serialise(byte[] data) throws TransmissionCapacityExceededException
	{
		String serialisedData = Base64.encodeBase64String(data);
		if(serialisedData.length() > MAX_BODY_SIZE)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE + "), exceeded by " + (serialisedData.length() - MAX_BODY_SIZE) + " characters");
	}

	@Override
	protected byte[] deserialise() throws IOException
	{
		if(body == null || body.isEmpty())
			throw new IllegalStateException("Transmission body is not set or empty.");
		return Base64.decodeBase64(body);
	}

	@Override
	public int getMaxPayloadBytes()
	{
		// TODO work out a reasonable size limit for HTTP transmissions
		return 0;
	}
	
}
