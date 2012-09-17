package uk.ac.ucl.excites.transmission.lib.model.schema;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mstevens
 *
 */
public class Schema
{

	private int id;
	private List<FieldType> fieldTypes;
	
	public Schema(int id)
	{
		this.id = id;
		fieldTypes = new ArrayList<FieldType>();
	}
	
	

}
