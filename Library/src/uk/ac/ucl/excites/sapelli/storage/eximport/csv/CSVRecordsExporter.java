/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport.csv;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileWriter;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser;


/**
 * Class to export {@link Record}s to XML files, which can be re-imported by {@link XMLRecordsImporter}.
 * 
 * @author mstevens
 */
public class CSVRecordsExporter extends SimpleSchemaTraverser implements Exporter
{

	// STATICS-------------------------------------------------------
	static public final String DEFAULT_SEPARATOR = ","; 
	
	// DYNAMICS------------------------------------------------------
	private File exportFolder;
	private String separator;
	
	private FileWriter writer = null;
	private List<ColumnPointer> columnPointers;
	
	public CSVRecordsExporter(String exportFolderPath)
	{
		this(new File(exportFolderPath), DEFAULT_SEPARATOR);
	}
	
	public CSVRecordsExporter(String exportFolderPath, String separator)
	{
		this(new File(exportFolderPath), separator);
	}
	
	public CSVRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_SEPARATOR);
	}
	
	public CSVRecordsExporter(File exportFolder, String separator)
	{
		if(!FileHelpers.createFolder(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.separator = separator;
		this.columnPointers = new ArrayList<ColumnPointer>();
	}
	
	private void openWriter(String description) throws Exception
	{
		Charset utf8 = Charset.forName("UTF-8");
		writer = new FileWriter(exportFolder + File.separator + "RecordDump_" + description + "_" + TimeUtils.getTimestampForFileName() + ".csv", utf8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
	}
	
	private void closeWriter()
	{
		writer.dispose(); // also closes
		writer = null;
	}
	
	/**
	 * @param records all assumed to be of the same top-level schema
	 * @param name
	 * @return
	 * @throws Exception
	 */
	@Override
	public ExportResult export(List<Record> records, String name) throws Exception
	{
		//TODO escape separator
		//TODO sort per schema
		//TODO append schemaID to header
		//TODO polish
		
		openWriter(name);
		int count = 0;
		
		if(!records.isEmpty())
		{
			// Construct column list:
			columnPointers.clear();
			traverse(records.get(0).getSchema());
			
			// Write header:
			writer.openTransaction(); // output will be buffered
			try
			{
				for(ColumnPointer cp : columnPointers)
					writer.write((!writer.isTransactionBufferEmpty() ? separator : "") + cp.getQualifiedColumnName());
				writer.write('\n');
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				writer.rollbackTransaction(); // !!!
				writer.delete(); // delete the file
				writer = null;
				return null;
			}
			writer.commitTransaction(); // write out buffer
			
			// Write records:
			for(Record r : records)
			{
				writer.openTransaction(); // output will be buffered
				try
				{	
					for(ColumnPointer cp : columnPointers)
					{
						if(!writer.isTransactionBufferEmpty())
							writer.write(separator);
						Column<?> col = cp.getColumn();
						Record rec = cp.getRecord(r, false);
						if(rec != null && col.isValueSet(rec))
							writer.write(col.retrieveValueAsString(rec));
					}
					writer.write('\n');				
				}
				catch(Exception e)
				{
					e.printStackTrace(System.err);
					writer.rollbackTransaction(); // !!!
					continue; // !!!
				}
				writer.commitTransaction(); // write out buffer
				count++;
			}
			
		}		
		ExportResult result = new ExportResult(count, writer.getFullPath());
		closeWriter();
		return result;
	}
	
	@Override
	public void visit(ColumnPointer leafColumnPointer)
	{
		columnPointers.add(leafColumnPointer);
	}
	
	@Override
	public boolean allowLocationSelfTraversal()
	{
		return true;
	}

	@Override
	public boolean allowOrientationSelfTraversal()
	{
		return true;
	}

	@Override
	public boolean allowForeignKeySelfTraversal()
	{
		return true;
	}

	@Override
	public boolean skipNonBinaryStoredLocationColumns()
	{
		return false;
	}

	@Override
	public boolean skipNonBinaryStoredOrientationColumns()
	{
		return false;
	}

	@Override
	public boolean includeVirtualColumns()
	{
		return true;
	}
	
}
