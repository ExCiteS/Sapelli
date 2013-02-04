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

import uk.ac.ucl.excites.collector.model.Choice;
import uk.ac.ucl.excites.collector.model.Field;
import uk.ac.ucl.excites.collector.model.Form;
import uk.ac.ucl.excites.collector.model.LocationField;
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
	static private final String Field_NO_COLUMN = "noColumn";

	private Project project;
	private Form currentForm;
	private String currentFormStartFieldID;
	private Choice currentChoice;
	private HashMap<Field, String> fieldToJumpId;
	private Hashtable<String, Field> idToField;

	public Project ParseProject(File xmlFile)
	{
		project = null;
		fieldToJumpId = new HashMap<Field, String>();
		idToField = new Hashtable<String, Field>();
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
	public void endDocument() throws SAXException {
		Log.i(TAG, "End document");
		// Resolve jumps...
		for (Entry<Field, String> jump : fieldToJumpId.entrySet()) {
			Field target = idToField.get(jump.getValue());
			if (target == null)
				Log.e(TAG, "Cannot resolve jump ID " + jump.getValue());
			else
				jump.getKey().setJump(target);
		}
	}

	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) throws SAXException
	{

		if (qName.equals("ExCiteS-Collector-Project")) {
			project = new Project();
		}

		if (qName.equals("Data-Management")) {
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
			if (attributes.getValue("startField") != null)
				currentFormStartFieldID = attributes.getValue("startField");
			else
				Log.w(TAG, "No startField attribute, will use first field");
			// TODO other attributes
		}

		if (qName.equals("Choice"))
		{
			Choice parent = currentChoice;
			currentChoice = new Choice(parent); // old currentChoice becomes the parent (if it is null that's ok)
			if (parent != null)
				parent.addChild(currentChoice); // add new choice as child of parent
			else
				currentForm.addField(currentChoice); //this is a top-level Choice, so add it as a field of the form
			//ID & jump
			setIDAndJump(currentChoice, attributes);
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
			LocationField locField = new LocationField();
			currentForm.addField(locField);
			setIDAndJump(locField, attributes);
			//TODO other attributes
		}

		if (qName.equals("Photo"))
		{
			// TODO
		}

		if (qName.equals("Audio"))
		{
			// TODO
		}
	}
	
	private void setIDAndJump(Field f, Attributes attributes)
	{
		if(attributes.getValue("id") != null)
		{
			f.setID(attributes.getValue("id"));
			idToField.put(f.getID(), f);
		}
		if (attributes.getValue("jump") != null)
			fieldToJumpId.put(currentChoice, attributes.getValue("jump"));
		if (currentFormStartFieldID == null) {
			if (currentForm.getStart() == null) // no startID was specified and the start field is not set yet
				currentForm.setStart(f);
		}
		else if(currentFormStartFieldID.equals(f.getID()))
			currentForm.setStart(f);
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
	
	private SAXException attribMissing(String tag, String attribute)
	{
		return new SAXException(attribute + " is missing, this is a required attribute of " + tag + ".");
	}
	
}
