/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport.xml;

import java.io.File;
import java.nio.charset.Charset;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.db.DataAccess;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
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
public class XMLRecordsExporter extends SimpleSchemaTraverser
{

	// STATICS-------------------------------------------------------
	static public final String TAG_RECORDS_EXPORT = "RecordsExport";
	static public final boolean USES_XML_VERSION_11 = false; // whether to use XML v1.0 (false) or XML v1.1 (true)

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
	
	// DYNAMICS------------------------------------------------------
	private File exportFolder;
	private DataAccess dao;
	private CompositeMode compositeMode;
	
	private FileWriter writer = null;
	private int tabs = 0;
	
	private Record currentRecord;
	
	public XMLRecordsExporter(String exportFolderPath, DataAccess dao, CompositeMode compositeMode)
	{
		this(new File(exportFolderPath), dao, compositeMode);
	}
	
	public XMLRecordsExporter(File exportFolder, DataAccess dao, CompositeMode compositeMode)
	{
		if(!FileHelpers.createFolder(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.dao = dao;
		this.compositeMode = compositeMode;
		this.currentRecord = null;
	}
	
	private void openWriter(String description) throws Exception
	{
		Charset utf8 = Charset.forName("UTF-8");
		writer = new FileWriter(exportFolder + File.separator + "RecordDump_" + description + "_" + TimeUtils.getTimestampForFileName() + ".xml", utf8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		writer.writeLine(XMLUtils.header(utf8.displayName(), USES_XML_VERSION_11));
		writer.writeLine("<" + TAG_RECORDS_EXPORT + ">");
		//TODO add attributes: exportDateTime, comment, device(?)
		tabs = 1;
	}
	
	private void closeWriter()
	{
		writer.writeLine("</" + TAG_RECORDS_EXPORT + ">");
		writer.close();
	}
	
	public int exportAll() throws Exception
	{
		openWriter("ALL");
		int count = export(dao.retrieveRecords());
		closeWriter();
		return count;
	}
	
	public int exportRecords(List<Record> records) throws Exception
	{
		return exportRecords(records, "Selection");
	}
	
	public int exportRecords(List<Record> records, String name) throws Exception
	{
		openWriter(name);
		int count = export(records);
		closeWriter();
		return count;
	}
	
	public int exportRecordsOf(List<Schema> schemas, String name) throws Exception
	{
		openWriter(name);
		int count = 0;
		for(Schema s : schemas)
			count += export(dao.retrieveRecords(s));
		closeWriter();
		return count;
	}
	
	public int exportRecordsOf(Schema s) throws Exception
	{
		openWriter(s.getName());
		int count = export(dao.retrieveRecords(s));
		closeWriter();
		return count;
	}
	
	private int export(List<Record> records)
	{
		int count = 0;
		for(Record r : records)
		{
			try
			{				
				//Open tag:
				writer.writeLine(StringUtils.addTabsFront("<" + Record.TAG_RECORD + " " +
						Schema.ATTRIBUTE_SCHEMA_NAME + "=\"" + XMLUtils.escapeCharacters(r.getSchema().getName()) + "\" " +
						Schema.ATTRIBUTE_SCHEMA_ID + "=\"" + r.getSchema().getID() + "\"" +
						">", tabs));
					//TODO transmission/sent
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				writer.writeLine(XMLUtils.comment("Exception on exporting record: " + e.toString() + (e.getMessage() != null ? " [" + e.getMessage() + "]" : ""), tabs));
				continue;
			}
			
			// Indent: 
			tabs++;
			
			// Traverse columns:			
			try
			{
				currentRecord = r;
				traverse(r.getSchema());
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				writer.writeLine(XMLUtils.comment("Exception on exporting record column values: " + e.toString() + (e.getMessage() != null ? " [" + e.getMessage() + "]" : ""), tabs));
				tabs = 2;
			}
			
			// Unindent:
			tabs--;
			
			//Close tag:
			writer.writeLine(StringUtils.addTabsFront("</" + Record.TAG_RECORD + ">", tabs));
			
			count++;
		}
		return count;
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
		Column<?> leafColumn = leafColumnPointer.getColumn();
		Record record = leafColumnPointer.getRecord(currentRecord, false);
		
		// If in nested or flat tags mode and subrecord is null: return
		if(record == null && compositeMode != CompositeMode.As_String)
			return;
		
		// Write column value or null value comment:
		String columnName = compositeMode == CompositeMode.As_flat_tags ? leafColumnPointer.getQualifiedColumnName() : leafColumn.getName();
		if(record != null && record.isValueSet(leafColumn))
			writer.writeLine(StringUtils.addTabsFront("<" + columnName + ">" + XMLUtils.escapeCharacters(leafColumn.retrieveValueAsString(record)) + "</" + columnName + ">", tabs));
		else
			writer.writeLine(StringUtils.addTabsFront(getNullRecordComment(columnName), tabs));
	}
	
	private String getNullRecordComment(String columnName)
	{
		return XMLUtils.comment(columnName + " is null");
	}
	
	@Override
	public boolean isLocationSelfTraversalAllowed()
	{
		return compositeMode != CompositeMode.As_String;
	}

	@Override
	public boolean isOrientationSelfTraversalAllowed()
	{
		return compositeMode != CompositeMode.As_String;
	}

	@Override
	public boolean isForeignKeySelfTraversalAllowed()
	{
		return compositeMode != CompositeMode.As_String;
	}

	@Override
	public boolean isSkippingNonBinaryStoredLocationColumnsAllowed()
	{
		return false;
	}

	@Override
	public boolean isSkippingNonBinaryStoredOrientationColumnsAllowed()
	{
		return false;
	}
	
	public String getFilePath()
	{
		if(writer != null)
			return writer.getFullPath();
		return null;
	}
	
	public String getFileName()
	{
		if(writer != null)
			return writer.getFileName();
		return null;
	}
	
}