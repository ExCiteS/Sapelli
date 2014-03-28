/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Subclass of Record to represent records create by the Sapelli Collector
 * 
 * @author mstevens
 */
public class CollectorRecord extends Record implements Comparable<CollectorRecord>
{

	static private final long serialVersionUID = 2L;
	
	private Form form;
	
	/**
	 * @param form
	 */
	public CollectorRecord(Form form)
	{
		super(form.getSchema());
		if(!form.isProducesRecords())
			throw new IllegalArgumentException("This form does not produce records!");
		this.form = form;
	}
	
	public DateTime getStartTime()
	{
		return getStartTime(false);
	}
	
	public DateTime getStartTime(boolean asStoredBinary)
	{
		if(asStoredBinary)
			return Form.COLUMN_TIMESTAMP_START.retrieveValueAsStoredBinary(this);
		else
			return Form.COLUMN_TIMESTAMP_START.retrieveValue(this);
	}
	
	public DateTime getEndTime()
	{
		if(form.isStoreEndTime())
			return Form.COLUMN_TIMESTAMP_END.retrieveValue(this);
		else
			return null;
	}
	
	public long getDeviceID()
	{
		return Form.COLUMN_DEVICE_ID.retrieveValue(this);
	}
	
	/**
	 * Returns the selected choice for the first ChoiceField in the form
	 * 
	 * @return the selected choice
	 */
	public ChoiceField getFirstSelectedChoice()
	{
		for(Field f : form.getFields())
			if(f instanceof ChoiceField)
				return ((ChoiceField) f).getSelectedChoice(this);
		throw new IllegalStateException("This form has no ChoiceField");
	}
	
	/**
	 * Sorts formEntries by startDate
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(CollectorRecord another)
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
