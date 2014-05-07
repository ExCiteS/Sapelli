/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport.xml;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser;


/**
 * Class to export {@link Record}s to XML files, which can be re-imported by {@link XMLRecordsImporter}.
 * 
 * @author mstevens
 */
public class XMLRecordsExporter extends SimpleSchemaTraverser implements Exporter
{

	// STATICS-------------------------------------------------------
	static public final String TAG_RECORDS_EXPORT = "RecordsExport";
	static public final boolean USES_XML_VERSION_11 = false; // whether to use XML v1.0 (false) or XML v1.1 (true)
	static private Charset UTF8 = Charset.forName("UTF-8");

	/**
	 * Different ways of representing composite columns (i.e. {@link RecordColumn}s)
	 */
	static public enum CompositeMode
	{
		/**
		 * Create one tag with name {@link RecordColumn#getName()} containing a single String value generated using {@link RecordColumn#toString(Record)}.
		 */
		As_String,
		
		/**
		 * Create separate tags for each "leaf" column, names are constructed from parents' names and the leaf column's name, separated by {@link RecordColumn#QUALIFIED_NAME_SEPARATOR}.
		 */
		As_flat_tags,
		
		/**
		 * Create one tag with name {@link RecordColumn#getName()} containing child tags for the subcolumn.
		 */
		As_nested_tags
	}
	
	static public CompositeMode DEFAULT_COMPOSITE_MODE = CompositeMode.As_flat_tags;
	
	// DYNAMICS------------------------------------------------------
	private File exportFolder;
	private CompositeMode compositeMode;
	
	private FileWriter writer = null;
	private int tabs = 0;
	
	private Record currentRecord;
	
	public XMLRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_COMPOSITE_MODE);
	}
	
	public XMLRecordsExporter(File exportFolder, CompositeMode compositeMode)
	{
		if(!FileHelpers.createFolder(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.compositeMode = compositeMode;
		this.currentRecord = null;
	}
	
	private void openWriter(String description) throws Exception
	{
		DateTime timestamp = DateTime.now();
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + ".xml"), UTF8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		writer.writeLine(XMLUtils.header(UTF8.displayName(), USES_XML_VERSION_11));
		writer.writeLine("<" + TAG_RECORDS_EXPORT + " exportedAt=\"" + TimeUtils.getISOTimestamp(timestamp, false) + "\">");
		//TODO add attributes: comment, device(?)
	}
	
	private void closeWriter()
	{
		if(writer != null)
		{
			writer.writeLine("</" + TAG_RECORDS_EXPORT + ">");
			writer.dispose(); // also closes
			writer = null;
		}
	}
	
	@Override
	public ExportResult export(List<Record> records, String description)
	{
		if(records == null || records.isEmpty())
			return ExportResult.NothingToExport();
		List<Record> exported = new ArrayList<Record>();
		try
		{
			openWriter(description);
			tabs = 1;
			for(Record r : records)
			{
				writer.openTransaction(); // output will be buffered
				try
				{				
					//Open tag:
					writer.writeLine(StringUtils.addTabsFront("<" + Record.TAG_RECORD + " " +
							Schema.ATTRIBUTE_SCHEMA_NAME + "=\"" + XMLUtils.escapeCharacters(r.getSchema().getName()) + "\" " +
							Schema.ATTRIBUTE_SCHEMA_ID + "=\"" + r.getSchema().getID() + "\"" +
							">", tabs));
							//TODO transmission/sent
				
					// Indent: 
					tabs++;
				
					// Traverse columns:
					currentRecord = r;
					traverse(r.getSchema());
				
					// Unindent:
					tabs--;
				
					//Close tag:
					writer.writeLine(StringUtils.addTabsFront("</" + Record.TAG_RECORD + ">", tabs));
				
				}
				catch(Exception e)
				{
					writer.rollbackTransaction(); // !!!
					tabs = 1;
					writer.writeLine(XMLUtils.comment("Exception on exporting record: " + e.toString() + (e.getMessage() != null ? " [" + e.getMessage() + "]" : ""), tabs));
					throw e; //!!!
				}
				writer.commitTransaction(); // write out buffer
				exported.add(r);
				// TODO mark record as exported?
			}
			// Result...			
			return ExportResult.Success(exported, exportFolder, Collections.singletonList(writer.getFile()));
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			if(!exported.isEmpty())
				return ExportResult.PartialFailure(exported, exportFolder, Collections.singletonList(writer.getFile()), e);
			else
				return ExportResult.Failure(e, exportFolder);
		}
		finally
		{
			closeWriter();
		}
	}

	@Override
	public void enter(RecordColumn<?> recordCol)
	{
		if(compositeMode == CompositeMode.As_String)
			return; //this should never happen
		
		// Push on columnStack:
		super.enter(recordCol);
		
		// Write null value comment if subrecord is null:
		if(getColumnPointer().retrieveValue(currentRecord) == null)
			writer.writeLine(StringUtils.addTabsFront(getNullRecordComment(recordCol.getName()), tabs));
		else if(compositeMode == CompositeMode.As_nested_tags) 
		{	// If in nested tags mode and subrecord is not null, open parent tag:
			writer.writeLine(StringUtils.addTabsFront("<" + recordCol.getName() + ">", tabs));
			tabs++;
		}
	}
	
	@Override
	public void leave(RecordColumn<?> recordCol)
	{
		if(compositeMode == CompositeMode.As_String)
			return; //this should never happen

		// If in nested tags mode and subrecord is not null, close parent tag:
		if(compositeMode == CompositeMode.As_nested_tags && getColumnPointer().retrieveValue(currentRecord) != null)
		{
			tabs--;
			writer.writeLine(StringUtils.addTabsFront("</" + recordCol.getName() + ">", tabs));
		}
		
		// Pop columnStack:
		super.leave(recordCol);
	}
	
	@Override
	public void visit(ColumnPointer leafColumnPointer)
	{
		Record record = leafColumnPointer.getRecord(currentRecord, false);
		
		// If in nested or flat tags mode and subrecord is null: return
		if(record == null && compositeMode != CompositeMode.As_String)
			return;
		
		// Write column value or null value comment:
		Column<?> leafColumn = leafColumnPointer.getColumn();
		String columnName = (compositeMode == CompositeMode.As_flat_tags ? leafColumnPointer.getQualifiedColumnName() : leafColumn.getName());
		if(record != null && leafColumn.isValueSet(record))
		{
			/* 	If the column type is String (meaning it is a StringColumn or a VirtualColumn with a StringColumn as its target), then
				we use the raw (i.e. unquoted) String value. We can do this because the difference between null and the empty string
				is preserved due to the fact that we do not put a tag (only an XML comment) in case the value is null.
				See XMLRecordsImporter#characters(char[], int, int) for the corresponding import logic.
				*/
			String valueString = (leafColumn.getType() == String.class ? (String) leafColumn.retrieveValue(record) : leafColumn.retrieveValueAsString(record));
			writer.writeLine(StringUtils.addTabsFront("<" + columnName + ">" + XMLUtils.escapeCharacters(valueString) + "</" + columnName + ">", tabs));
		}
		else
			writer.writeLine(StringUtils.addTabsFront(getNullRecordComment(columnName), tabs));
	}
	
	private String getNullRecordComment(String columnName)
	{
		return XMLUtils.comment(columnName + " is null");
	}
	
	@Override
	public boolean allowLocationSelfTraversal()
	{
		return compositeMode != CompositeMode.As_String;
	}

	@Override
	public boolean allowOrientationSelfTraversal()
	{
		return compositeMode != CompositeMode.As_String;
	}

	@Override
	public boolean allowForeignKeySelfTraversal()
	{
		return compositeMode != CompositeMode.As_String;
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
