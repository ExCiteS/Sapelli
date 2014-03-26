/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class ExImportHelper
{

	static public enum Format
	{
		XML,
		CSV
	}
	
	static private Exporter getExporter(File exportFolder, Format format, XMLRecordsExporter.CompositeMode xmlCompositeMode)
	{
		switch(format)
		{
			case XML :	return new XMLRecordsExporter(exportFolder, xmlCompositeMode);
			case CSV : return null; //TODO CSV exporter
			default: return null;
		}
	}
	
	static public ExportResult exportRecords(File exportFolder, List<Record> records, String exportName, Format format) throws Exception
	{
		return exportRecords(exportFolder, records, exportName, format, XMLRecordsExporter.DEFAULT_COMPOSITE_MODE);
	}
	
	static public ExportResult exportRecords(File exportFolder, List<Record> records, String exportName, Format format, XMLRecordsExporter.CompositeMode xmlCompositeMode) throws Exception
	{
		Exporter exporter = getExporter(exportFolder, format, xmlCompositeMode);
		if(exporter == null)
			throw new UnsupportedOperationException("Could not export records in format " + format.toString());
		return exporter.export(records, exportName);
	}
	
	//TODO import methods

}
