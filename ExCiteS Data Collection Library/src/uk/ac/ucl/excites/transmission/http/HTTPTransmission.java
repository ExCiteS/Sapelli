package uk.ac.ucl.excites.transmission.http;

import java.io.IOException;
import java.util.Set;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.BinaryTransmission;
import uk.ac.ucl.excites.transmission.DecodeException;
import uk.ac.ucl.excites.transmission.ModelProvider;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.TransmissionSender;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

public class HTTPTransmission extends BinaryTransmission
{
	
	private String serverURL;

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
	public HTTPTransmission(ModelProvider modelProvider)
	{
		super(modelProvider);
	}
	
	@Override
	protected void sendPayload(TransmissionSender transmissionSender) throws Exception
	{
		//HTTPClient client = sender.getHTTPClient();
		
		// TODO Auto-generated method stub
	}

	@Override
	protected void readPayload(Schema schemaToUse, Settings settingsToUse) throws IllegalStateException, IOException, DecodeException
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void serialise(byte[] data) throws TransmissionCapacityExceededException
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected byte[] deserialise() throws IOException
	{
		// TODO Auto-generated method stub
		return null;
	}
	
}
