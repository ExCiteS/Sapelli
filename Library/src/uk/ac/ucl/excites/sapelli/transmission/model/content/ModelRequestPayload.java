package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload that is sent to signify that the sender (of this payload) is unfamiliar with the model that the receiver has just sent records for.
 * 
 * TODO perhaps also include a list of the model IDs that *are* known?
 * 
 * @author benelliott
 */
public class ModelRequestPayload extends Payload
{

	private long unknownModelID;
	private Transmission<?> subject;
	
	public ModelRequestPayload(Transmission<?> subject, long unknownModelID)
	{
		this.unknownModelID = unknownModelID;
		this.subject = subject;
	}
	
	@Override
	public int getType()
	{
		return Payload.BuiltinType.ModelRequest.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		Model.MODEL_ID_FIELD.write(unknownModelID, bitstream);
	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		unknownModelID = Model.MODEL_ID_FIELD.readLong(bitstream);
	}

	@Override
	public boolean acknowledgeReception()
	{
		return true; // ?
	}
	
	public long getUnknownModelID()
	{
		return unknownModelID;
	}
}
