/**
 * 
 */
package uk.ac.ucl.excites.storage.xml;

import java.util.List;

import uk.ac.ucl.excites.util.XMLUtils;

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.util.FileHelpers;
import uk.ac.ucl.excites.util.FileWriter;
import uk.ac.ucl.excites.util.TimeUtils;


/**
 * @author mstevens
 *
 */
public class RecordsExporter
{

	static public final String TAG_RECORDS_EXPORT = "RecordsExport";
	
	private String exportFolderPath;
	private DataAccess dao;
	
	public RecordsExporter(String exportFolderPath, DataAccess dao)
	{
		if(!FileHelpers.createFolder(exportFolderPath))
			throw new IllegalArgumentException("Export folder (" + exportFolderPath + ") does not exist and could not be created!");
		this.exportFolderPath = exportFolderPath;
		this.dao = dao;
	}
	
	private FileWriter openWriter(String description) throws Exception
	{
		FileWriter writer = new FileWriter(exportFolderPath + "RecordDump_" + description + "_" + TimeUtils.getTimestampForFileName() + ".xml", "UTF-8");
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		writer.writeLine(XMLUtils.header());
		writer.writeLine("<" + TAG_RECORDS_EXPORT + ">");
		return writer;
	}
	
	private void closeWriter(FileWriter writer)
	{
		writer.writeLine("</" + TAG_RECORDS_EXPORT + ">");
		writer.close();
	}
	
	public void exportAll() throws Exception
	{
		FileWriter writer = openWriter("ALL");
		exportRecords(dao.retrieveRecords(), writer);
		closeWriter(writer);
	}
	
	public void export(List<Record> records) throws Exception
	{
		FileWriter writer = openWriter("Selection");
		exportRecords(records, writer);
		closeWriter(writer);
	}
	
	public void export(List<Schema> schemas, String name) throws Exception
	{
		FileWriter writer = openWriter(name);
		for(Schema s : schemas)
			exportRecords(dao.retrieveRecords(s), writer);
		closeWriter(writer);
	}
	
	public void export(Schema s) throws Exception
	{
		FileWriter writer = openWriter(s.getName() + "_" + s.getID() + "_v" + s.getVersion());
		exportRecords(dao.retrieveRecords(s), writer);
		closeWriter(writer);
	}
	
	private void exportRecords(List<Record> records, FileWriter writer)
	{
		for(Record r : records)
			writer.writeLine(r.toXML(1));
	}
	
}
