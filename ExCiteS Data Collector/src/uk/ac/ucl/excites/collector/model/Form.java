/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.storage.db.DataStorageAccess;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class Form
{
	
	//Statics--------------------------------------------------------
	public static final int END_ACTION_LOOP = 0;
	public static final int END_ACTION_EXIT = 1;
	public static final int END_ACTION_DEFAULT = END_ACTION_LOOP;
	
	//Dynamics-------------------------------------------------------
	private int schemaID;
	private int schemaVersion;
	private String name;
	
	//Fields
	private Field start;
	private ArrayList<Field> fields;
	private LocationField locationField;

	//Android shortcut:
	private boolean shortcut;
	//shortcutIcon 

	//End action:
	private int endAction;
	private boolean vibrateOnEnd;
	private String endSoundPath;
	
	//Buttons:
	private boolean showBack;
	private boolean showHome;
	
	public Form(int id)
	{
		this(id, null);
	}
	
	public Form(int id, String name)
	{
		this(id, Schema.DEFAULT_VERSION, name);
	}	
	
	public Form(int id, int version, String name)
	{
		this.schemaID = id;
		this.schemaVersion = version;
		this.name = name;
		this.fields = new ArrayList<Field>();
	}
	
	public void addField(Field f)
	{
		fields.add(f);
		if(f instanceof LocationField)
		{
			if(locationField != null)
				throw new IllegalStateException("For now we only support 1 Location field per Form (this may change in the future).");
			locationField = (LocationField) f;
		}
	}
	
	public List<Field> getFields()
	{
		return fields;
	}

	/**
	 * @return the start
	 */
	public Field getStart()
	{
		return start;
	}

	/**
	 * @param start the start to set
	 */
	public void setStart(Field start)
	{
		this.start = start;
	}
	
	public boolean hasLocationField()
	{
		return locationField != null; 
	}
	
	public LocationField getLocationField()
	{
		return locationField;
	}
	
	/**
	 * @return the endAction
	 */
	public int getEndAction()
	{
		return endAction;
	}

	/**
	 * @return the vibrateOnEnd
	 */
	public boolean isVibrateOnEnd()
	{
		return vibrateOnEnd;
	}

	/**
	 * @return the endSoundPath
	 */
	public String getEndSoundPath()
	{
		return endSoundPath;
	}

	public Schema getSchema(DataStorageAccess dsa)
	{
		Schema schema = dsa.retrieveSchema(schemaID, schemaVersion);
		if(schema == null)
		{
			schema = new Schema(schemaID, schemaVersion, name);			
			for(Field f : fields)
				if(!f.isNoColumn())
					f.addColumns(schema);
			schema.seal();
			dsa.store(schema);
		}
		return schema;
	}
	
}
