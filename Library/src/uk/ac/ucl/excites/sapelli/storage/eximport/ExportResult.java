/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class ExportResult
{
	
	// STATICS-------------------------------------------------------
	static public ExportResult Success(List<Record> exportedRecords, String destination)
	{
		return new ExportResult(exportedRecords, destination, null, null);
	}
	
	static public ExportResult Success(List<Record> exportedRecords, File folder, List<File> files)
	{
		return new ExportResult(exportedRecords, folder.getAbsolutePath(), files, null);
	}
	
	static public ExportResult PartialFailure(List<Record> exportedRecords, String destination, Exception reason)
	{
		return new ExportResult(exportedRecords, destination, null, reason);
	}
	
	static public ExportResult PartialFailure(List<Record> exportedRecords, File folder, List<File> files, Exception reason)
	{
		return new ExportResult(exportedRecords, folder.getAbsolutePath(), files, reason);
	}
	
	static public ExportResult Failure(Exception reason, String destination)
	{
		return new ExportResult(null, destination, null, reason);
	}
	
	static public ExportResult Failure(Exception reason, File folder)
	{
		return new ExportResult(null, folder.getAbsolutePath(), null, reason);
	}
	
	static public ExportResult NothingToExport()
	{
		return new ExportResult(null, "", null, null);
	}
	
	// DYNAMICS------------------------------------------------------
	private final List<Record> exportedRecords;
	private final String destination;
	private final List<File> files;
	private final Exception failureReason;
	
	/**
	 * @param exportedRecords
	 * @param destination
	 * @param files
	 * @param failureReason
	 */
	private ExportResult(List<Record> exportedRecords, String destination, List<File> files, Exception failureReason)
	{
		this.exportedRecords = exportedRecords;
		this.destination = destination;
		this.files = files;
		this.failureReason = failureReason;
	}

	/**
	 * @return the numberedOfExportedRecords
	 */
	public int getNumberedOfExportedRecords()
	{
		return getExportedRecords().size();
	}

	/**
	 * @return the successfully exported records
	 */
	public List<Record> getExportedRecords()
	{
		return exportedRecords == null ? Collections.<Record> emptyList() : exportedRecords;
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
		return files == null ? Collections.<File> emptyList() : files;
	}
	
	public Exception getFailureReason()
	{
		return failureReason;
	}
	
	public String getMessage()
	{
		if(failureReason == null)
		{
			if(getNumberedOfExportedRecords() > 0)
				return String.format(Locale.getDefault(), "Succesfully exported %d records to %s.", getNumberedOfExportedRecords(), destination); //TODO multilang
			else
				return "There was nothing to export."; //TODO multilang
		}
		else if(getNumberedOfExportedRecords() > 0)
			return String.format(Locale.getDefault(), "Succesfully exported %d records to %s, failed to export 1 or more additional records due to %s", getNumberedOfExportedRecords(), destination, failureReason.getMessage() != null ? failureReason.getMessage() : failureReason); //TODO multilang
		else
			return String.format(Locale.getDefault(), "Failed to export records to %s, due to %s.", destination, failureReason.getMessage() != null ? failureReason.getMessage() : failureReason); //TODO multilang
	}
	
	public boolean wasSuccessful()
	{
		return failureReason == null;
	}

}
