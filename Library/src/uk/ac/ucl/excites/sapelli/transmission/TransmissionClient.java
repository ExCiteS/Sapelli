/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;

/**
 * @author mstevens
 *
 */
public interface TransmissionClient extends StorageClient
{

	public EncryptionSettings getEncryptionSettingsFor(long modelID);
	
}
