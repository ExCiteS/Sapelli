package uk.ac.ucl.excites.transmission.http;

import java.io.IOException;
import java.util.Set;

import org.apache.commons.codec.binary.Base64;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.BinaryTransmission;
import uk.ac.ucl.excites.transmission.ModelProvider;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.TransmissionSender;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

public class HTTPTransmission extends BinaryTransmission
{
	
	public static final int MAX_BODY_SIZE = 4096; //TODO determine a good value
	
	private String serverURL;
	private String body = null;
	
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param serverURL
	 * @param settings
	 */
	public HTTPTransmission(Schema schema, String serverURL, Settings settings)
	{
		this(schema, null, serverURL, settings);
	}
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param serverURL
	 * @param settings
	 */
	public HTTPTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, String serverURL, Settings settings)
	{
		super(schema, columnsToFactorOut, settings);
		this.serverURL = serverURL;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param modelProvider
	 */
	public HTTPTransmission(DateTime receivedAt, ModelProvider modelProvider)
	{
		super(modelProvider);
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
	
}
