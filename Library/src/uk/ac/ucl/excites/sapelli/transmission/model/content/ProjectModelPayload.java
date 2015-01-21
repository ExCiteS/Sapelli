package uk.ac.ucl.excites.sapelli.transmission.model.content;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload that it used to transmit a project's model so that the receiver of this payload can decode records of schemata belonging to that model.
 * 
 * @author benelliott
 */
public class ProjectModelPayload extends Payload
{
	
	private Model model;
	
	public ProjectModelPayload(Model model) throws IOException
	{
		this.model = model;
	}
	
	@Override
	public int getType()
	{
		return Payload.BuiltinType.ProjectModel.ordinal();
	}

	@Override
	protected void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		// TODO

	}

	@Override
	protected void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException
	{
		// TODO
	}

	@Override
	public boolean acknowledgeReception()
	{
		return true;
	}
	
	public Model getModel()
	{
		return model;
	}

}
