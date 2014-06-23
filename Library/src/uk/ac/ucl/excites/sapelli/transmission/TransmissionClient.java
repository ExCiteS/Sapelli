/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * @author mstevens
 *
 */
public abstract class TransmissionClient extends StorageClient
{

	// STATICS-------------------------------------------------------
	static public final long TRANSMISSION_MANAGEMENT_MODEL_ID = 0; // reserved!
	static public final Model TRANSMISSION_MANAGEMENT_MODEL = new Model(TRANSMISSION_MANAGEMENT_MODEL_ID, "TransmissionManagement");

	// DYNAMICS------------------------------------------------------
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getReserveredModels()
	 */
	@Override
	public List<Model> getReserveredModels()
	{
		List<Model> reserved = super.getReserveredModels();
		reserved.add(TRANSMISSION_MANAGEMENT_MODEL);
		return reserved;
	}
	
	public abstract EncryptionSettings getEncryptionSettingsFor(Model model) throws UnknownModelException;
	
	public abstract Payload newPayload(int nonBuiltinType);
	
}
