/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport.csv;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileWriter;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser;


/**
 * Class to export {@link Record}s to XML files, which can be re-imported by {@link XMLRecordsImporter}.
 * 
 * The CSV will get a header with the column names separated by the separator, plus this:
 * 	*separator*schemaID=...*separator*
 * 
 * @author mstevens
 */
public class CSVRecordsExporter extends SimpleSchemaTraverser implements Exporter
{

	// STATICS------------------------------------------------------- 
	static private Charset UTF8 = Charset.forName("UTF-8");
	
	static public enum Separator
	{
		
		COMMA,
		SEMICOLON,
		TAB;
		
		/**
		 * TODO use i18n/StringProvide
		 * 
		 * @see java.lang.Enum#toString()
		 */
		@Override
		public String toString()
		{
			return name().toLowerCase(Locale.getDefault());
		}
		
		public String getSeparatorString()
		{
			switch(this)
			{
				case COMMA:
					return ",";
				case SEMICOLON:
					return ";";
				case TAB:
					return "\t";
				default:
					throw new IllegalStateException("Unknown CSV sepator: " + name());
			}
		}
		
	}
	
	static public final Separator DEFAULT_SEPARATOR = Separator.COMMA;
	
	// DYNAMICS------------------------------------------------------
	private File exportFolder;
	private Separator separator;
	
	private FileWriter writer = null;
	private List<ColumnPointer> columnPointers;
	
	public CSVRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_SEPARATOR);
	}
	
	public CSVRecordsExporter(File exportFolder, Separator separator)
	{
		if(!FileHelpers.createFolder(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.separator = separator;
		this.columnPointers = new ArrayList<ColumnPointer>();
	}
	
	private void openWriter(String description, DateTime timestamp) throws Exception
	{
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + ".csv"), UTF8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);	
	}
	
	private void deleteFile()
	{
		if(writer != null)
			writer.delete();
		closeWriter();
	}
	
	private void closeWriter()
	{
		if(writer != null)
		{
			writer.dispose(); // also closes
			writer = null;
		}
	}
	
	/**
	 * @param records all assumed to be of the same top-level schema
	 * @param description
	 * @return
	 * @throws Exception
	 */
	@Override
	public ExportResult export(List<Record> records, String description)
	{
		if(records == null || records.isEmpty())
			return ExportResult.NothingToExport();
		
		// Timestamp for filenames:
		DateTime timestamp = DateTime.now();
		
		// Group records by schema:
		Map<Schema, List<Record>> recordsBySchema = new HashMap<Schema, List<Record>>();
		for(Record r : records)
		{
			List<Record> recordsForSchema;
			if(recordsBySchema.containsKey(r.getSchema()))
				recordsForSchema = recordsBySchema.get(r.getSchema());
			else
			{
				recordsForSchema = new ArrayList<Record>();
				recordsBySchema.put(r.getSchema(), recordsForSchema);
			}
			recordsForSchema.add(r);
		}
		
		// Export each group to a separate CSV file:
		List<Record> exported = new ArrayList<Record>();
		List<File> csvFiles = new ArrayList<File>();
		for(Map.Entry<Schema, List<Record>> entry : recordsBySchema.entrySet())
		{
			try
			{
				Schema schema =  entry.getKey();
				openWriter(description + "_" + schema.getName(), timestamp);

				// Construct column list:
				columnPointers.clear();
				traverse(schema);
				
				// Write header:
				writer.openTransaction(); // output will be buffered
				try
				{
					// Column names (separatoed by the separator):
					for(ColumnPointer cp : columnPointers)
						writer.write((!writer.isTransactionBufferEmpty() ? separator.getSeparatorString() : "") + cp.getQualifiedColumnName());
					// Postfix: separator+"modelID="+...+separator+"modelSchemaNumber="+...+separator
					writer.write(	separator.getSeparatorString() + Schema.ATTRIBUTE_MODEL_ID + "=" + schema.getModelID() +
									separator.getSeparatorString() + Schema.ATTRIBUTE_MODEL_SCHEMA_NUMBER + "=" + schema.getModelSchemaNumber() +
									separator.getSeparatorString());
					writer.write('\n');
				}
				catch(Exception e)
				{
					writer.rollbackTransaction(); // !!!
					throw e;
				}
				writer.commitTransaction(); // write out buffer
				
				// Write records:
				for(Record r : entry.getValue())
				{
					if(r.getSchema().isInternal())
						continue; // we do not export records of internal schemata
					writer.openTransaction(); // output will be buffered
					try
					{	
						for(ColumnPointer cp : columnPointers)
						{
							if(!writer.isTransactionBufferEmpty())
								writer.write(separator.getSeparatorString());
							Column<?> col = cp.getColumn();
							Record rec = cp.getRecord(r, false);
							if(rec != null && col.isValueSet(rec))
							{
								writer.write(	(col instanceof VirtualColumn && ((VirtualColumn<?, ?>) col).getTargetColumn() instanceof StringColumn) ?				
													(String) col.retrieveValue(rec) :
													col.retrieveValueAsString(rec));
								// TODO escape separator!
								// TODO escape white space!
								/* If the column is a VirtualColumn and its target column is a StringColumn then we can print the value as an unquoted String.
								 * Unlike in the case of XMLRecordsExporter we can only do this for virtual StringColumns, because those will never be parsed
								 * by CSVRecordsImporter anyway. If we'd do it for normal StringColumns (which would be parser upon importing) then we'd lose
								 * the different between null and the empty String (which are not the same for StringColumns). */
							}
						}
						writer.write('\n');
					}
					catch(Exception e)
					{
						writer.rollbackTransaction(); // !!!
						throw e;
					}
					writer.commitTransaction(); // write out buffer
					exported.add(r);
					// TODO mark record as exported?
				}
				csvFiles.add(writer.getFile());
				closeWriter();
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				deleteFile();
				if(!exported.isEmpty())
					return ExportResult.PartialFailure(exported, exportFolder, csvFiles, e, records.size() - exported.size());
				else
					return ExportResult.Failure(exportFolder, e, records.size());
			}
		}
		// Success:
		return ExportResult.Success(exported, exportFolder, csvFiles);
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
	public boolean skipNonBinarySerialisedLocationSubColumns()
	{
		return false;
	}

	@Override
	public boolean skipNonBinarySerialisedOrientationSubColumns()
	{
		return false;
	}

	@Override
	public boolean includeVirtualColumns()
	{
		return true;
	}
	
}
