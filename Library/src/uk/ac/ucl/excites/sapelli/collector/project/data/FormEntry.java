/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.data;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * Simple wrapper around Record to make Form-specific columns/values more easily accessible
 * 
 * @author mstevens
 *
 */
public class FormEntry implements Comparable<FormEntry>
{

	private Form form;
	private Record record;
	private Schema schema;
	
	public FormEntry(Form form, Record record)
	{
		if(!record.getSchema().equals(form.getSchema()))
			throw new IllegalArgumentException("Schema mismatch!");
		this.form = form;
		this.record = record;
		this.schema = form.getSchema();
	}
	
	public DateTime getStartTime()
	{
		return getStartTime(false);
	}
	
	public DateTime getStartTime(boolean asStoredBinary)
	{
		DateTimeColumn dtCol = (DateTimeColumn) schema.getColumn(Form.COLUMN_TIMESTAMP_START);
		if(asStoredBinary)
			return dtCol.retrieveValueAsStoredBinary(record);
		else
			return dtCol.retrieveValue(record);
	}
	
	public DateTime getEndTime()
	{
		if(form.isStoreEndTime())
			return ((DateTimeColumn) schema.getColumn(Form.COLUMN_TIMESTAMP_END)).retrieveValue(record);
		else
			return null;
	}
	
	public long getDeviceID()
	{
		return ((IntegerColumn) schema.getColumn(Form.COLUMN_DEVICE_ID)).retrieveValue(record);
	}
	
	/**
	 * Returns the selected choice for the first ChoiceField in the form
	 * 
	 * @return the selected choice
	 */
	public ChoiceField getSelectedChoice()
	{
		for(Field f : form.getFields())
			if(f instanceof ChoiceField)
				return getSelectedChoice((ChoiceField) f);
		throw new IllegalStateException("This form has no ChoiceField");
	}

	/**
	 * Returns the selected choice for the named (by ID) ChoiceField
	 * 
	 * @param fieldID  choiceField ID String
	 * @return the selected choice
	 */
	public ChoiceField getSelectedChoice(String fieldID)
	{
		Field field = form.getField(fieldID);
		if(field == null)
			throw new IllegalArgumentException("Field \"" + fieldID + "\" does not exist.");
		if(field instanceof ChoiceField)
			return getSelectedChoice((ChoiceField) field);
		else
			throw new IllegalArgumentException("Field \"" + fieldID + "\" is not a ChoiceField.");
	}
	
	/**
	 * Returns the selected choice for the given ChoiceField 
	 * 
	 * @param rootChoiceField the choiceField
	 * @return the selected choice
	 */
	public ChoiceField getSelectedChoice(ChoiceField rootChoiceField)
	{
		if(rootChoiceField.isNoColumn())
			throw new IllegalArgumentException("Field \"" + rootChoiceField.getID() + "\" has no column.");
		Long choiceIdx = (Long) schema.getColumn(rootChoiceField.getID()).retrieveValue(record);
		if(choiceIdx != null)
			return rootChoiceField.getDictionary().getChoice(choiceIdx.intValue());
		else
			return null;
	}

	/**
	 * Sorts formEntries by startDate
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(FormEntry another)
	{
		return this.getStartTime().compareTo(another.getStartTime());
	}

	/**
	 * @return the form
	 */
	public Form getForm()
	{
		return form;
	}

	/**
	 * @return the record
	 */
	public Record getRecord()
	{
		return record;
	}

	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}
	
//	/**
//	 * This is rather hackish, but somehow I prefer it over duplicating the {@link Record#toXML(int)} code entirely here
//	 * 
//	 * @param tabs
//	 * @return
//	 */
//	public String toXML(int tabs)
//	{
//		// Look-up selected choices:
//		Map<String, ChoiceField> choiceFieldIDToSelected = new HashMap<String, ChoiceField>();
//		for(Field f : form.getFields())
//			if(f instanceof ChoiceField && !f.isNoColumn())
//				choiceFieldIDToSelected.put(f.getID(), getSelectedChoice((ChoiceField) f));
//		
//		// Process XML:
//		String[] lines = record.toXML(tabs).split("\n");
//		StringBuilder bldr = new StringBuilder();
//		Pattern closeTagPattern = Pattern.compile("</(.+)>");
//		bldr.append(lines[0] + "\n"); //<Record>
//		for(int l = 1; l < lines.length - 1; l++) //skip <Record> & </Record>
//		{
//			//Add line as is:
//			bldr.append(lines[l] + "\n");
//			
//			//Check close tag and at "<*-value>...</*-value>" line if needed:
//			Matcher matcher = closeTagPattern.matcher(lines[l]);
//			if(matcher.find())
//			{
//				String columnName = matcher.group(1);
//				if(choiceFieldIDToSelected.containsKey(columnName))
//				{
//					ChoiceField selected = choiceFieldIDToSelected.get(columnName); 
//					bldr.append(StringUtils.addTabsFront("<" + columnName + "-value>" + (selected != null ? XMLUtils.escapeCharacters(selected.getValue()) : null) + "</" + columnName + "-value>\n", tabs + 1));
//				}
//			}
//		}
//		bldr.append(lines[lines.length - 1] + "\n"); //</Record>
//		return bldr.toString();
//	}
	
	public String toKML()
	{
		//TODO KML output
		return "";
	}
	
}
