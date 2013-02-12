/**
 * 
 */
package uk.ac.ucl.excites.collector.xml;

import java.io.File;
import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import uk.ac.ucl.excites.collector.model.Audio;
import uk.ac.ucl.excites.collector.model.Choice;
import uk.ac.ucl.excites.collector.model.Field;
import uk.ac.ucl.excites.collector.model.Form;
import uk.ac.ucl.excites.collector.model.LocationField;
import uk.ac.ucl.excites.collector.model.MediaAttachment;
import uk.ac.ucl.excites.collector.model.Photo;
import uk.ac.ucl.excites.collector.model.Project;
import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

/**
 * @author mstevens, julia
 *
 */
public class ProjectParser extends DefaultHandler
{

	static private String TAG = "PROJECT PARSER";
	static private final String FORM = "Form";
	static private final String FORM_SCHEMA_ID = "schema-id";
	static private final String FORM_SCHEMA_VERSION = "schema-versiob";
	static private final String FORM_START_FIELD = "startField";
	static private final String Field_NO_COLUMN = "noColumn";

	private Project project;
	private Form currentForm;
	private String currentFormStartFieldID;
	private Choice currentChoice;
	private HashMap<Field, String> fieldToJumpId;
	private Hashtable<String, Field> idToField;
	private HashMap<MediaAttachment, String> mediaAttachToDisableId;

	public Project parseProject(File xmlFile)
	{
		project = null;
		fieldToJumpId = new HashMap<Field, String>();
		idToField = new Hashtable<String, Field>();
		mediaAttachToDisableId = new HashMap<MediaAttachment, String>();
		try
		{
			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();
			XMLReader xr = sp.getXMLReader();
			FileInputStream fis = new FileInputStream(xmlFile);
			xr.setContentHandler(this);
			xr.parse(new InputSource(fis));
		}
		catch(Exception e)
		{
			Log.e(TAG, "XML Parsing Exception = " + e);
			return null; //System.exit(-1);
		}
		return project;
	}

	@Override
	public void startDocument() throws SAXException {
		Log.i(TAG, "Start document");
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		if (qName.equals("ExCiteS-Collector-Project"))
		{
			project = new Project();
		}

		if (qName.equals("Data-Management"))
		{
			// TODO
		}
		
		if(qName.equals(FORM))
		{
			if(attributes.getValue(FORM_SCHEMA_ID) == null)
				throw attribMissing(FORM, FORM_SCHEMA_ID);
			int schemaID = Integer.parseInt(attributes.getValue(FORM_SCHEMA_ID));			
			int schemaVersion = (attributes.getValue(FORM_SCHEMA_VERSION) == null ? Schema.DEFAULT_VERSION : Integer.parseInt(attributes.getValue(FORM_SCHEMA_VERSION)));
			currentForm = new Form(schemaID, schemaVersion, attributes.getValue("name"));
			project.addForm(currentForm);
			if(attributes.getValue(FORM_START_FIELD) != null)
				currentFormStartFieldID = attributes.getValue(FORM_START_FIELD);
			else
				Log.w(TAG, "No startField attribute, will use first field");
			// TODO other attributes
		}

		if (qName.equals("Choice"))
		{
			currentChoice = new Choice(attributes.getValue("id"), currentChoice); //old currentChoice becomes the parent (if it is null that's ok)
			if(currentChoice.isRoot())
				currentForm.addField(currentChoice); //this is a top-level Choice, so add it as a field of the form
			//Remember ID & jumps
			rememberIDAndJump(currentChoice, attributes);
			//No column:
			currentChoice.setNoColumn(attributes.getValue(Field_NO_COLUMN) != null && attributes.getValue(Field_NO_COLUMN).equalsIgnoreCase("true"));
			//Other attributes:
			if (attributes.getValue("img") != null)
				currentChoice.setImagePath(attributes.getValue("img"));
			if (attributes.getValue("cols") != null)
				currentChoice.setCols(Integer.parseInt(attributes.getValue("cols")));
			if (attributes.getValue("rows") != null)
				currentChoice.setCols(Integer.parseInt(attributes.getValue("rows")));
			if (attributes.getValue("value") != null)
				currentChoice.setValue(attributes.getValue("value"));
			//...
		}

		if (qName.equals("Location"))
		{
			LocationField locField = new LocationField(attributes.getValue("id"));
			currentForm.addField(locField);
			rememberIDAndJump(locField, attributes);
			locField.setTimeoutS(attributes.getValue("timeout") == null ? LocationField.DEFAULT_TIMEOUT_S : Integer.parseInt(attributes.getValue("timeout")));
			//Type:
			String type = attributes.getValue("type");
			if(type != null)
				Log.w(TAG, "Unknown Location type (" + type + ").");
			else
			{	
				if("GPS".equalsIgnoreCase(type))
					locField.setType(LocationField.TYPE_GPS);
				else if("Network".equalsIgnoreCase(type))
					locField.setType(LocationField.TYPE_GPS);
			}
			//Storage settings:
			locField.setDoublePrecision(readBooleanAttribute(attributes, "doublePrecision", LocationField.DEFAULT_DOUBLE_PRECISION));
			locField.setStoreAltitude(readBooleanAttribute(attributes, "storeAltitude", LocationField.DEFAULT_STORE_ALTITUDE));
			locField.setStoreBearing(readBooleanAttribute(attributes, "storeBearing", LocationField.DEFAULT_STORE_BEARING));
			locField.setStoreSpeed(readBooleanAttribute(attributes, "storeSpeed", LocationField.DEFAULT_STORE_SPEED));
			locField.setStoreAccuracy(readBooleanAttribute(attributes, "storeAccuracy", LocationField.DEFAULT_STORE_ACCURACY));
		}
		
		if (qName.equals("Photo"))
		{
			Photo photoField = new Photo(attributes.getValue("id"));
			currentForm.addField(photoField);
			rememberIDAndJump(photoField, attributes);
			mediaAttachmentAttributes(photoField, attributes);
			// TODO
		}

		if (qName.equals("Audio"))
		{
			Audio audioField = new Audio(attributes.getValue("id"));
			currentForm.addField(audioField);
			rememberIDAndJump(audioField, attributes);
			mediaAttachmentAttributes(audioField, attributes);
			// TODO button images
		}
	}
	
	private void mediaAttachmentAttributes(MediaAttachment ma, Attributes attributes)
	{
		ma.setMinMax(	(attributes.getValue("min") == null ?
							MediaAttachment.DEFAULT_MIN :
							Integer.parseInt(attributes.getValue("min"))),
						(attributes.getValue("max") == null ?
							MediaAttachment.DEFAULT_MAX :
							Integer.parseInt(attributes.getValue("max"))));
		if(attributes.getValue("disableField") != null)
			mediaAttachToDisableId.put(ma, attributes.getValue("disableField"));
	}
	
	private boolean readBooleanAttribute(Attributes attributes, String attributeName, boolean defaultValue)
	{
		String text = attributes.getValue(attributeName).trim();
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return text.equalsIgnoreCase(Boolean.TRUE.toString()) ? Boolean.TRUE : defaultValue;
		//We don't use Boolean.parseBoolean(String) because that returns false if the String does not equal true (so we wouldn't be able to return the defaultValue)
	}
	
	private void rememberIDAndJump(Field f, Attributes attributes)
	{
		//Remember ID:
		if(f.getID() != null)
		{
			if(idToField.get(f.getID()) != null)
				Log.w(TAG, "Duplicate field id (" + f.getID() + "!");
			idToField.put(f.getID(), f);
		}
		//Remember jump:
		if(attributes.getValue("jump") != null)
			fieldToJumpId.put(f, attributes.getValue("jump"));
		//Resolve/set form start field:
		if(currentFormStartFieldID == null)
		{
			if(currentForm.getStart() == null) //no startID was specified and the start field is not set yet
				currentForm.setStart(f); //set first field of the form as start field
		}
		else if(currentFormStartFieldID.equals(f.getID()))
			currentForm.setStart(f);
	}
	
	private void resolveJumpsAndDisablings()
	{
		// Resolve jumps...
		for(Entry<Field, String> jump : fieldToJumpId.entrySet())
		{
			Field target = idToField.get(jump.getValue());
			if (target == null)
				Log.e(TAG, "Cannot resolve jump ID " + jump.getValue());
			else
				jump.getKey().setJump(target);
		}
		//Resolve disabling of Choices by MediaAttachments...
		for(Entry<MediaAttachment, String> disable : mediaAttachToDisableId.entrySet())
		{
			Field target = idToField.get(disable.getValue());
			if(target == null)
				Log.e(TAG, "Cannot resolve disable field ID " + disable.getValue());
			else
				disable.getKey().setDisableChoice((Choice) target);
		}
	}
	
	@Override
	public void endElement(String uri, String localName, String qName)
	{
		if (qName.equals("Form"))
		{
			currentForm = null;
			currentFormStartFieldID = null;
		}

		if (qName.equals("Choice"))
			currentChoice = currentChoice.getParent();
	}
	
	@Override
	public void endDocument() throws SAXException
	{
		Log.i(TAG, "End document");
		resolveJumpsAndDisablings(); //!!!
	}
	
	private SAXException attribMissing(String tag, String attribute)
	{
		return new SAXException(attribute + " is missing, this is a required attribute of " + tag + ".");
	}
	
}
