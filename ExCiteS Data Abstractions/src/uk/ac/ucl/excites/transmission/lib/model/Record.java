package uk.ac.ucl.excites.transmission.lib.model;

import java.util.List;

import uk.ac.ucl.excites.transmission.lib.model.schema.*;

/**
 * @author mstevens
 *
 */
public class Record
{

	private Schema schema;
	private List<Field> fields;
	
	public Record(Schema schema)
	{
		this.schema = schema;
	}
	
	

}
