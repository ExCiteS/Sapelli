/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.io.File;
import java.util.Collections;
import java.util.List;

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
		return new ExportResult(exportedRecords, destination, null, null, 0);
	}
	
	static public ExportResult Success(List<Record> exportedRecords, File folder, List<File> files)
	{
		return new ExportResult(exportedRecords, folder.getAbsolutePath(), files, null, 0);
	}
	
	static public ExportResult PartialFailure(List<Record> exportedRecords, String destination, Exception reason, int numberOfUnexportedRecords)
	{
		return new ExportResult(exportedRecords, destination, null, reason, numberOfUnexportedRecords);
	}
	
	static public ExportResult PartialFailure(List<Record> exportedRecords, File folder, List<File> files, Exception reason, int numberOfUnexportedRecords)
	{
		return new ExportResult(exportedRecords, folder.getAbsolutePath(), files, reason, numberOfUnexportedRecords);
	}
	
	static public ExportResult Failure(String destination, Exception reason, int numberOfUnexportedRecords)
	{
		return new ExportResult(null, destination, null, reason, numberOfUnexportedRecords);
	}
	
	static public ExportResult Failure(File folder, Exception reason, int numberOfUnexportedRecords)
	{
		return new ExportResult(null, folder.getAbsolutePath(), null, reason, numberOfUnexportedRecords);
	}
	
	static public ExportResult NothingToExport()
	{
		return new ExportResult(null, "", null, null, 0);
	}
	
	// DYNAMICS------------------------------------------------------
	private final List<Record> exportedRecords;
	private final String destination;
	private final List<File> files;
	private final Exception failureReason;
	private final int numberOfUnexportedRecords;
	
	/**
	 * @param exportedRecords
	 * @param destination
	 * @param files
	 * @param failureReason
	 */
	private ExportResult(List<Record> exportedRecords, String destination, List<File> files, Exception failureReason, int numberOfUnexportedRecords)
	{
		this.exportedRecords = exportedRecords;
		this.destination = destination;
		this.files = files;
		this.failureReason = failureReason;
		this.numberOfUnexportedRecords = numberOfUnexportedRecords;
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
	
	public boolean wasSuccessful()
	{
		return failureReason == null && numberOfUnexportedRecords == 0;
	}

	/**
	 * @return the numberOfUnexportedRecords
	 */
	public int getNumberOfUnexportedRecords()
	{
		return numberOfUnexportedRecords;
	}
	
}
