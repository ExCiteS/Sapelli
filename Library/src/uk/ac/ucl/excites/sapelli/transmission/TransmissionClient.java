/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public interface TransmissionClient extends StorageClient
{

	//TODO replace by: public EncryptionSettings getEncryptionSettings(int usageID, int usageSubID) ... 
	public Settings getSettingsFor(Schema schema);
	
	public Set<Column<?>> getFactoredOutColumnsFor(Schema schema);
	
}
