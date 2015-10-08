/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public interface ExportFilenameProvider
{

	public String getExportFileName(Schema schema);
	
}
