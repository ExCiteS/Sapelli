/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * @author mstevens
 *
 */
public class ExportResult
{
	
	static public ExportResult Success(int numberedOfRecords, String destination)
	{
		ExportResult result = new ExportResult();
		result.numberedOfRecords = numberedOfRecords;
		result.destination = destination;
		return result;
	}
	
	static public ExportResult Success(int numberedOfRecords, File folder, List<File> files)
	{
		ExportResult result = new ExportResult();
		result.numberedOfRecords = numberedOfRecords;
		result.destination = folder.getAbsolutePath();
		result.files = files;
		return result;
	}
	
	static public ExportResult PartialFailure(int numberedOfRecords, String destination, Exception reason)
	{
		ExportResult result = new ExportResult();
		result.numberedOfRecords = numberedOfRecords;
		result.destination = destination;
		result.failureReason = reason;
		return result;
	}
	
	static public ExportResult PartialFailure(int numberedOfRecords, File folder, List<File> files, Exception reason)
	{
		ExportResult result = new ExportResult();
		result.numberedOfRecords = numberedOfRecords;
		result.destination = folder.getAbsolutePath();
		result.files = files;
		result.failureReason = reason;
		return result;
	}
	
	static public ExportResult Failure(Exception reason, String destination)
	{
		ExportResult result = new ExportResult();
		result.failureReason = reason;
		result.destination = destination;
		return result;
	}
	
	static public ExportResult Failure(Exception reason, File folder)
	{
		ExportResult result = new ExportResult();
		result.failureReason = reason;
		result.destination = folder.getAbsolutePath();
		return result;
	}
	
	private int numberedOfRecords = 0;
	private String destination;
	private List<File> files;
	private Exception failureReason = null;
	
	private ExportResult() {}
	
	/**
	 * @return the numberedOfExportedRecords
	 */
	public int getNumberedOfExportedRecords()
	{
		return numberedOfRecords;
	}

	/**
	 * @return the destination
	 */
	public String getDestination()
	{
		return destination;
	}
	
	public List<File> getFiles()
	{
		if(files == null)
			return Collections.<File> emptyList();
		return files;
	}
	
	public Exception getFailureReason()
	{
		return failureReason;
	}
	
	public String getMessage()
	{
		if(failureReason == null)
			return String.format(Locale.getDefault(), "Succesfully exported %d records to %s.", numberedOfRecords, destination); //TODO multilang
		else if(numberedOfRecords > 0)
			return String.format(Locale.getDefault(), "Succesfully exported %d records to %s, failed to export 1 or more additional records due to %s", numberedOfRecords, destination, failureReason); //TODO multilang
		else
			return String.format(Locale.getDefault(), "Failed to export records to %s, due to %s.", destination, failureReason); //TODO multilang
	}

}
