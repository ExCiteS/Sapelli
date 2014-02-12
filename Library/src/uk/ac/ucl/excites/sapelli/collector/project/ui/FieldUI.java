package uk.ac.ucl.excites.sapelli.collector.project.ui;

import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

public interface FieldUI
{
	
	public Field getField();

	public void update(Record record);
	
	public void cancel();
	
	public boolean isValid(Record record);
	
	public void storeValue(Record record);
	
}
