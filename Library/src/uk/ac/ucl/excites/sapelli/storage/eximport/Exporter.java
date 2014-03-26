package uk.ac.ucl.excites.sapelli.storage.eximport;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

public interface Exporter
{

	public ExportResult export(List<Record> records, String name) throws Exception;
	
}
