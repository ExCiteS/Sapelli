/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms.text;

import java.util.List;
import java.util.Set;
import java.util.SortedSet;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.SchemaProvider;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 */
public class TextSMSTransmission extends SMSTransmission
{
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public TextSMSTransmission(Schema schema, byte id, SMSAgent receiver, Settings settings)
	{
		super(schema, null, id, receiver, settings);
	}

	/**
	 * To be called on the sending side.
	 * 
	 * @param schema
	 * @param columnsToFactorOut
	 * @param id
	 * @param receiver
	 * @param settings
	 */
	public TextSMSTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, byte id, SMSAgent receiver, Settings settings)
	{
		super(schema, columnsToFactorOut, id, receiver, settings);
	}

	/**
	 * To be called on the receiving side.
	 * 
	 * @param schemaProvider
	 * @param settings
	 */
	public TextSMSTransmission(SchemaProvider schemaProvider, Settings settings)
	{
		super(schemaProvider, settings);
	}
	
	@Override
	protected List<Message> serialiseAndSplit(byte[] data) throws TransmissionCapacityExceededException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected byte[] mergeAndDeserialise(SortedSet<Message> parts)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
