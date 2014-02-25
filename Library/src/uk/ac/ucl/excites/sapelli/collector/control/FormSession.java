/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.control;

import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class FormSession
{

	public static enum Mode
	{
		CREATE,
		EDIT,
		//SELECT
	}
	
	static public FormSession Create(Form form, long deviceIDHash)
	{
		return new FormSession(form, Mode.CREATE, form.isProducesRecords() ? form.newEntry(deviceIDHash) : null);
	}
	
	static public FormSession Edit(Form form, Record record)
	{
		return new FormSession(form, Mode.EDIT, record);
	}
	
	//Dynamic
	/*package*/ Form form;
	/*package*/ Mode mode;
	/*package*/ Record record;
	/*package*/ Stack<Field> fieldHistory;
	
	
	/**
	 * @param form
	 * @param mode
	 * @param record
	 */
	private FormSession(Form form, Mode mode, Record record)
	{
		this.form = form;
		this.mode = mode;
		this.record = record;
		this.fieldHistory = new Stack<Field>();
	}

	/**
	 * @return the form
	 */
	public Form getForm()
	{
		return form;
	}

	/**
	 * @return the mode
	 */
	public Mode getMode()
	{
		return mode;
	}

	/**
	 * @return the record
	 */
	public Record getRecord()
	{
		return record;
	}

	/**
	 * @return the fieldHistory
	 */
	public Stack<Field> getFieldHistory()
	{
		return fieldHistory;
	}
	
}
