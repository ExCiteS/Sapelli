package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

public interface Exporter
{

	static public enum Format
	{
		XML,
		CSV
	}
	
	public ExportResult export(List<Record> records, String description);
	
}
