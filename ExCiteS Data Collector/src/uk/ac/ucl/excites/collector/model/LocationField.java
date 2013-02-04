/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.Schema;



/**
 * @author mstevens
 *
 */
public class LocationField extends Field
{

	public static final int TYPE_GPS = 1;
	//...

	private int type;
	private float accuracy;
	private int timeoutS;
	
	
	@Override
	protected void _addColumns(Schema schema)
	{
		//TODO
		
	}
	
	
	
	
}
