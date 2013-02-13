/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class FormEntry extends Record
{

	private Form form;
	private DataAccess dao;
	
	/**
	 * @param schema
	 */
	public FormEntry(Form form, DataAccess dao)
	{
		super(form.getSchema(dao));
		this.form = form;
		this.dao = dao;
	}
	
	public void store()
	{
		//TODO store in db
	}
	
}
