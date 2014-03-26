/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

/**
 * @author mstevens
 *
 */
public class ExportResult
{
	
	private int numberedOfExportedRecords;
	private String destination;
	
	/**
	 * @param numberedOfExportedRecords
	 * @param destination
	 */
	public ExportResult(int numberedOfExportedRecords, String destination)
	{
		this.numberedOfExportedRecords = numberedOfExportedRecords;
		this.destination = destination;
	}
	
	/**
	 * @return the numberedOfExportedRecords
	 */
	public int getNumberedOfExportedRecords()
	{
		return numberedOfExportedRecords;
	}

	/**
	 * @return the destination
	 */
	public String getDestination()
	{
		return destination;
	}

}
