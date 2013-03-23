/**
 *
 */
package uk.ac.ucl.excites.collector.project.xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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

import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.CancelField;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Field.Optionalness;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.MediaField;
import uk.ac.ucl.excites.collector.project.model.OrientationField;
import uk.ac.ucl.excites.collector.project.model.PhotoField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;

/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public class ProjectParser extends DefaultHandler
{

	// STATICS--------------------------------------------------------
	// Tags:
	static private final String TAG_PROJECT = "ExCiteS-Collector-Project";
	static private final String TAG_CONFIGURATION = "Configuration";
	static private final String TAG_TRANSMISSION = "Transmission";
	static private final String TAG_DROPBOX_UPLOAD = "DropboxUpload";
	static private final String TAG_HTTP_UPLOAD = "HTTPUpload";
	static private final String TAG_SMS_UPLOAD = "SMSUpload";
	static private final String TAG_ENCRYPTION = "Encryption";
	static private final String TAG_ALLOW_MOBILE_DATA = "AllowMobileData";
	static private final String TAG_ALLOW_ROAMING = "AllowRoaming";
	static private final String TAG_LOGGING = "Logging";
	static private final String TAG_FORM = "Form";
	private static final String TAG_CHOICE = "Choice";
	static private final String TAG_AUDIO = "Audio";
	static private final String TAG_PHOTO = "Photo";
	static private final String TAG_ORIENTATION = "Orientation";

	// Attributes:
	static private final String ATTRIBUTE_PROJECT_NAME = "name";
	static private final String ATTRIBUTE_PROJECT_VERSION = "version";
	static private final String ATTRIBUTE_ENABLED = "enabled";
	static private final String ATTRIBUTE_FORM_NAME = "name";
	static private final String ATTRIBUTE_FORM_SCHEMA_ID = "schema-id";
	static private final String ATTRIBUTE_FORM_SCHEMA_VERSION = "schema-version";
	static private final String ATTRIBUTE_FORM_START_FIELD = "startField";
	static private final String ATTRIBUTE_FORM_END_SOUND = "endSound";
	static private final String ATTRIBUTE_FORM_SHORTCUT_IMAGE = "shortcutImage";
	static private final String ATTRIBUTE_FIELD_ID = "id";
	static private final String ATTRIBUTE_FIELD_JUMP = "jump";
	static private final String ATTRIBUTE_FIELD_NO_COLUMN = "noColumn";
	static private final String ATTRIBUTE_DISABLE_FIELD = "disableField";
	private static final String ATTRIBUTE_SHOW_FORWARD = "showForward";
	private static final String ATTRIBUTE_SHOW_CANCEL = "showCancel";
	private static final String ATTRIBUTE_SHOW_BACK = "showBack";

	// DYNAMICS-------------------------------------------------------
	private final String basePath;
	private final boolean createProjectFolder;
	private Project project;
	private Settings transmissionSettings;
	private Form currentForm;
	private String currentFormStartFieldId;
	private ChoiceField currentChoice;
	private HashMap<Field, String> fieldToJumpId;
	private Hashtable<String, Field> idToField;
	private HashMap<MediaField, String> mediaAttachToDisableId;
	private boolean inConfigTag = false;

	public ProjectParser(String basePath, boolean createProjectFolder)
	{
		this.basePath = basePath;
		this.createProjectFolder = createProjectFolder;
	}

	public Project parseProject(File xmlFile) throws Exception
	{
		if(xmlFile == null || !xmlFile.exists() || xmlFile.length() == 0)
			throw new IllegalArgumentException("Invalid xmlFile (" + (xmlFile == null ? "null" : xmlFile.getAbsolutePath()) + ")!");
		return parseProject(new FileInputStream(xmlFile));
	}

	public Project parseProject(InputStream input) throws Exception
	{
		if(input == null)
			throw new IllegalArgumentException("Invalid input stream");
		project = null;
		fieldToJumpId = new HashMap<Field, String>();
		idToField = new Hashtable<String, Field>();
		mediaAttachToDisableId = new HashMap<MediaField, String>();
		try
		{
			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();
			XMLReader xr = sp.getXMLReader();
			xr.setContentHandler(this);
			xr.parse(new InputSource(input));
		}
		catch(Exception e)
		{
			System.err.println("XML Parsing Exception = " + e);
			// e.printStackTrace(System.err);
			// return null;
			throw e;
		}
		finally
		{
			try
			{
				input.close();
			}
			catch(IOException ioe)
			{
				ioe.printStackTrace();
			}
		}
		return project;
	}

	@Override
	public void startDocument() throws SAXException
	{
		// does nothing (for now)
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		// <ExCiteS-Collector-Project>
		if(qName.equals(TAG_PROJECT))
		{
			String projectName = readRequiredStringAttribute(TAG_PROJECT, attributes, ATTRIBUTE_PROJECT_NAME);
			project = new Project(projectName, readStringAttribute(attributes, ATTRIBUTE_PROJECT_VERSION, Project.DEFAULT_VERSION), basePath, createProjectFolder);
		}
		// <Configuration>
		else if(qName.equals(TAG_CONFIGURATION))
		{
			inConfigTag = true;
		}
		// <Transmission>
		else if(qName.equals(TAG_TRANSMISSION))
		{
			if(!inConfigTag)
				throw new SAXException("<" + TAG_TRANSMISSION + "> should only appear in <" + TAG_CONFIGURATION + ">.");
			if(project.getTransmissionSettings() != null)
				throw new SAXException("There can be only one <" + TAG_TRANSMISSION + "> tag.");
			transmissionSettings = new Settings();
			project.setTransmissionSettings(transmissionSettings);
		}
		// <DropboxUpload>
		else if(qName.equals(TAG_DROPBOX_UPLOAD))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_DROPBOX_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setDropboxUpload(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_DROPBOX_UPLOAD));
		}
		// <HTTPUpload>
		else if(qName.equals(TAG_HTTP_UPLOAD))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_HTTP_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setHTTPUpload(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_HTTP_UPLOAD));
			String server = attributes.getValue("server");
			if(server != null && !server.isEmpty())
				transmissionSettings.setServerAddress(server);
		}
		// <SMSUpload>
		else if(qName.equals(TAG_SMS_UPLOAD))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_SMS_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setSMSUpload(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_SMS_UPLOAD));
			String relay = attributes.getValue("relay");
			if(relay != null && !relay.isEmpty())
				transmissionSettings.setSMSRelay(new SMSAgent(relay));
		}
		// <Encryption>
		else if(qName.equals(TAG_ENCRYPTION))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_ENCRYPTION + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setEncrypt(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_ENCRYPT));
		}
		// <AllowMobileData>
		else if(qName.equals(TAG_ALLOW_MOBILE_DATA))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_ALLOW_MOBILE_DATA + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setAllowMobileData(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_ALLOW_MOBILE_DATA));
		}
		// <AllowRoaming>
		else if(qName.equals(TAG_ALLOW_ROAMING))
		{
			if(transmissionSettings == null)
				throw new SAXException("<" + TAG_ALLOW_ROAMING + "> should only appear in <" + TAG_TRANSMISSION + ">.");
			transmissionSettings.setAllowMobileData(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Settings.DEFAULT_ALLOW_ROAMING));
		}
		// <Logging>
		else if(qName.equals(TAG_LOGGING))
		{
			if(!inConfigTag)
				throw new SAXException("<" + TAG_LOGGING + "> should only appear in <" + TAG_CONFIGURATION + ">.");
			project.setLogging(readBooleanAttribute(attributes, ATTRIBUTE_ENABLED, Project.DEFAULT_LOGGING));
		}
		// <Form>
		else if(qName.equals(TAG_FORM))
		{
			String name = readRequiredStringAttribute(TAG_FORM, attributes, ATTRIBUTE_FORM_NAME);
			int schemaID = Integer.parseInt(readRequiredStringAttribute(TAG_FORM, attributes, ATTRIBUTE_FORM_SCHEMA_ID));
			int schemaVersion = readIntegerAttribute(attributes, ATTRIBUTE_FORM_SCHEMA_VERSION, Schema.DEFAULT_VERSION);
			currentForm = new Form(project, name, schemaID, schemaVersion);
			project.addForm(currentForm);
			// Shortcut image:
			currentForm.setShortcutImageLogicalPath(readStringAttribute(attributes, ATTRIBUTE_FORM_SHORTCUT_IMAGE, null));
			// Store end time?:
			currentForm.setStoreEndTime(readBooleanAttribute(attributes, "storeEndTime", Form.END_TIME_DEFAULT));
			// Sound end vibration at the end of the form:
			// Get the sound path
			currentForm.setEndSoundPath(readStringAttribute(attributes, ATTRIBUTE_FORM_END_SOUND, null));
			currentForm.setVibrateOnEnd(readBooleanAttribute(attributes, "endVibrate", Form.DEFAULT_VIBRATE));
			// Which buttons are allowed to show:
			currentForm.setShowBack(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_BACK, Form.DEFAULT_SHOW_BACK));
			currentForm.setShowCancel(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_CANCEL, Form.DEFAULT_SHOW_CANCEL));
			currentForm.setShowForward(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_FORWARD, Form.DEFAULT_SHOW_FORWARD));
			// Button images:
			currentForm.setBackButtonImageLogicalPath(attributes.getValue("backButtonImg"));
			currentForm.setCancelButtonImageLogicalPath(attributes.getValue("cancelButtonImg"));
			currentForm.setForwardButtonImageLogicalPath(attributes.getValue("forwardButtonImg"));
			// Button background colour:
			currentForm.setButtonBackgroundColor(readStringAttribute(attributes, "buttonBackgroundColor", Form.DEFAULT_BUTTON_BACKGROUND_COLOR));
			// Start field:
			if(attributes.getValue(ATTRIBUTE_FORM_START_FIELD) != null && !attributes.getValue(ATTRIBUTE_FORM_START_FIELD).isEmpty())
				currentFormStartFieldId = attributes.getValue(ATTRIBUTE_FORM_START_FIELD);
			else
				System.out.println("Warning: No startField attribute, will use first field");
		}
		// <Choice>
		else if(qName.equals(TAG_CHOICE))
		{
			currentChoice = new ChoiceField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), currentChoice); // old currentChoice becomes the parent (if it is null that's ok)
			newField(currentChoice, attributes);
			// No column:
			currentChoice.setNoColumn(readBooleanAttribute(attributes, ATTRIBUTE_FIELD_NO_COLUMN, Field.DEFAULT_NO_COLUMN));
			// Other attributes:
			if(attributes.getValue("img") != null)
				currentChoice.setImageLogicalPath(attributes.getValue("img"));
			currentChoice.setCols(readIntegerAttribute(attributes, "cols", ChoiceField.DEFAULT_NUM_COLS));

			if(attributes.getValue("rows") != null)
				currentChoice.setRows(Integer.parseInt(attributes.getValue("rows")));
			if(attributes.getValue("value") != null)
				currentChoice.setValue(attributes.getValue("value"));
			// ...
		}
		// <Location>
		else if(qName.equals("Location"))
		{
			LocationField locField = new LocationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
			newField(locField, attributes);

			// Type:
			String type = attributes.getValue("type");
			if(type != null)
				System.out.println("Warning: Unknown Location type (" + type + ").");
			else if("Any".equalsIgnoreCase(type))
				locField.setType(LocationField.TYPE_ANY);
			else if("GPS".equalsIgnoreCase(type))
				locField.setType(LocationField.TYPE_GPS);
			else if("Network".equalsIgnoreCase(type))
				locField.setType(LocationField.TYPE_GPS);
			// Operating settings:
			locField.setStartWithForm(readBooleanAttribute(attributes, "startWithForm", LocationField.DEFAULT_START_WITH_FORM));
			locField.setWaitAtField(readBooleanAttribute(attributes, "waitAtField", LocationField.DEFAULT_WAIT_AT_FIELD));
			locField.setTimeoutS(readIntegerAttribute(attributes, "timeout", LocationField.DEFAULT_TIMEOUT_S));
			locField.setMaxAgeS(readIntegerAttribute(attributes, "maxAge", LocationField.DEFAULT_MAX_AGE_S));
			locField.setMaxAccuracyRadius(readFloatAttribute(attributes, "maxAccuracyRadius", LocationField.DEFAULT_MAX_ACCURACY_RADIUS));
			locField.setUseBestNonQualifyingLocationAfterTimeout(readBooleanAttribute(attributes, "useBestKnownLocationOnTimeout",
					LocationField.DEFAULT_USE_BEST_NON_QUALIFYING_LOCATION_AFTER_TIMEOUT));
			// Storage settings:
			locField.setDoublePrecision(readBooleanAttribute(attributes, "doublePrecision", LocationField.DEFAULT_DOUBLE_PRECISION));
			locField.setStoreAltitude(readBooleanAttribute(attributes, "storeAltitude", LocationField.DEFAULT_STORE_ALTITUDE));
			locField.setStoreBearing(readBooleanAttribute(attributes, "storeBearing", LocationField.DEFAULT_STORE_BEARING));
			locField.setStoreSpeed(readBooleanAttribute(attributes, "storeSpeed", LocationField.DEFAULT_STORE_SPEED));
			locField.setStoreAccuracy(readBooleanAttribute(attributes, "storeAccuracy", LocationField.DEFAULT_STORE_ACCURACY));
			locField.setStoreProvider(readBooleanAttribute(attributes, "storeProvider", LocationField.DEFAULT_STORE_PROVIDER));
		}
		// <Photo>
		else if(qName.equals(TAG_PHOTO))
		{
			PhotoField photoField = new PhotoField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
			newField(photoField, attributes);
			mediaAttachmentAttributes(photoField, attributes);
			photoField.setUseNativeApp(readBooleanAttribute(attributes, "useNativeApp", PhotoField.DEFAULT_USE_NATIVE_APP));
			// Camera options (only used when useNativeApp=false):
			photoField.setUseFrontFacingCamera(readBooleanAttribute(attributes, "useFrontCamera", PhotoField.DEFAULT_USE_FRONT_FACING_CAMERA));
			String flashText = attributes.getValue("flash");
			PhotoField.FlashMode flash = PhotoField.DEFAULT_FLASH_MODE;
			if(flashText != null && !flashText.isEmpty())
			{
				flashText = flashText.trim();
				if(flashText.equalsIgnoreCase("on") || flashText.equalsIgnoreCase("always") || flashText.equalsIgnoreCase("true"))
					flash = PhotoField.FlashMode.ON;
				else if(flashText.equalsIgnoreCase("auto"))
					flash = PhotoField.FlashMode.AUTO;
				else if(flashText.equalsIgnoreCase("off") || flashText.equalsIgnoreCase("never") || flashText.equalsIgnoreCase("false"))
					flash = PhotoField.FlashMode.OFF;
			}
			photoField.setFlashMode(flash);
			// Custom buttons (only used when useNativeApp=false):
			photoField.setCaptureButtonImageLogicalPath(attributes.getValue("captureImg"));
			photoField.setApproveButtonImageLogicalPath(attributes.getValue("approveImg"));
			photoField.setDiscardButtonImageLogicalPath(attributes.getValue("discardImg"));
		}
		// <Audio>
		else if(qName.equals(TAG_AUDIO))
		{
			AudioField audioField = new AudioField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
			newField(audioField, attributes);
			mediaAttachmentAttributes(audioField, attributes);
			audioField.setStartRecImageLogicalPath(attributes.getValue("startRecImg"));
			audioField.setStopRecImageLogicalPath(attributes.getValue("stopRecImg"));
		}
		// <Orientation>
		else if(qName.equals(TAG_ORIENTATION))
		{
			OrientationField orField = new OrientationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
			newField(orField, attributes);
			orField.setStoreAzimuth(readBooleanAttribute(attributes, "storeAzimuth", OrientationField.DEFAULT_STORE_AZIMUTH));
			orField.setStoreAzimuth(readBooleanAttribute(attributes, "storePitch", OrientationField.DEFAULT_STORE_PITCH));
			orField.setStoreAzimuth(readBooleanAttribute(attributes, "storeRoll", OrientationField.DEFAULT_STORE_ROLL));
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException
	{
		// </ExCiteS-Collector-Project>
		if(qName.equals(TAG_PROJECT))
		{
			if(project.getForms().size() == 0)
				throw new SAXException("A project such have at least 1 form!");
		}
		// </Configuration>
		else if(qName.equals(TAG_CONFIGURATION))
		{
			inConfigTag = false;
		}
		// </Transmission>
		else if(qName.equals(TAG_TRANSMISSION))
		{
			transmissionSettings = null;
		}
		// </Form>
		else if(qName.equals(TAG_FORM))
		{
			currentForm.initialiseStorage(); // generates Schema, Column & ValueDictionaries
			currentForm = null;
			currentFormStartFieldId = null;
		}
		// </Choice>
		else if(qName.equals(TAG_CHOICE))
		{
			if(currentChoice.isRoot() && currentChoice.isLeaf())
				throw new SAXException("Root choices need at least 1 child (but 2 children probably makes more sense).");
			currentChoice = currentChoice.getParent();
		}
	}

	/**
	 * Adds field to current form, sets optionalness, remembers id & jump
	 * 
	 * @param f
	 * @param attributes
	 */
	private void newField(Field f, Attributes attributes)
	{
		if(f.isRoot())
		{
			currentForm.addField(f);
			setOptionalness(f, attributes);
		}
		rememberIDAndJump(f, attributes);
		// Which buttons are allowed to show:
		f.setShowBack(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_BACK, Field.DEFAULT_SHOW_BACK));
		f.setShowCancel(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_CANCEL, Field.DEFAULT_SHOW_CANCEL));
		f.setShowForward(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_FORWARD, Field.DEFAULT_SHOW_FORWARD));
	}

	private void mediaAttachmentAttributes(MediaField ma, Attributes attributes)
	{
		setOptionalness(ma, attributes);
		ma.setMax(readIntegerAttribute(attributes, "max", MediaField.DEFAULT_MAX));
		if(attributes.getValue(ATTRIBUTE_DISABLE_FIELD) != null)
			mediaAttachToDisableId.put(ma, attributes.getValue(ATTRIBUTE_DISABLE_FIELD).trim());
	}

	private void rememberIDAndJump(Field f, Attributes attributes)
	{
		// Remember ID:
		if(f.getID() != null)
		{
			if(idToField.get(f.getID()) != null)
				System.out.println("Warning: Duplicate field id (" + f.getID() + "!");
			idToField.put(f.getID(), f);
		}
		// Remember jump:
		if(attributes.getValue(ATTRIBUTE_FIELD_JUMP) != null)
		{
			String jumpToId = attributes.getValue(ATTRIBUTE_FIELD_JUMP).trim();
			//make _END form-specific:
			if(jumpToId.equalsIgnoreCase(EndField.ID))
				jumpToId = EndField.ID(f.getForm());
			//make _CANCEL form-specific:
			if(jumpToId.equalsIgnoreCase(CancelField.ID))
				jumpToId = CancelField.ID(f.getForm());
			//Store field & jumpToId:
			fieldToJumpId.put(f, jumpToId);
		}
		// Resolve/set form start field:
		if(currentFormStartFieldId == null)
		{
			if(currentForm.getStartField() == null) // no startID was specified and the start field is not set yet
				currentForm.setStartField(f); // set first field of the form as start field
		}
		else if(currentFormStartFieldId.equals(f.getID()))
			currentForm.setStartField(f);
	}

	private void resolveReferences()
	{
		// Add an EndField and an CancelField instance so that _END and _CANCEL jumps can be resolved
		for(Form f : project.getForms())
		{
			EndField endF = new EndField(f);
			idToField.put(endF.getID(), endF);
			CancelField cancelF = new CancelField(f);
			idToField.put(cancelF.getID(), cancelF);
		}
		// Resolve jumps...
		for(Entry<Field, String> jump : fieldToJumpId.entrySet())
		{
			Field target = idToField.get(jump.getValue());
			if(target == null)
				System.out.println("Warning: Cannot resolve jump ID " + jump.getValue());
			else
				jump.getKey().setJump(target);
		}
		// Resolve disabling of Choices by MediaAttachments...
		for(Entry<MediaField, String> disable : mediaAttachToDisableId.entrySet())
		{
			Field target = idToField.get(disable.getValue());
			if(target == null)
				System.out.println("Warning: Cannot resolve disable field ID " + disable.getValue());
			else
				disable.getKey().setDisableChoice((ChoiceField) target);
		}
	}

	@Override
	public void endDocument() throws SAXException
	{
		resolveReferences(); // !!!
	}

	protected void setOptionalness(Field field, Attributes attributes)
	{
		String optText = attributes.getValue("optional");
		Optionalness opt = Field.DEFAULT_OPTIONAL;
		if(optText != null && !optText.isEmpty())
			if(optText.trim().equalsIgnoreCase("always") || optText.trim().equalsIgnoreCase("true"))
				opt = Optionalness.ALWAYS;
			else if(optText.trim().equalsIgnoreCase("notIfReached"))
				opt = Optionalness.NOT_IF_REACHED;
			else if(optText.trim().equalsIgnoreCase("never") || optText.trim().equalsIgnoreCase("false"))
				opt = Optionalness.NEVER;
		field.setOptional(opt);
	}

	protected String readRequiredStringAttribute(String qName, Attributes attributes, String attributeName) throws SAXException
	{
		String value = attributes.getValue(attributeName);
		if(value == null)
			throw new SAXException(attributeName + " is missing, this is a required attribute of " + qName + ".");
		return value;
	}

	protected String readStringAttribute(Attributes attributes, String attributeName, String defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return text;
	}

	protected boolean readBooleanAttribute(Attributes attributes, String attributeName, boolean defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else if(text.trim().equalsIgnoreCase(Boolean.TRUE.toString()))
			return Boolean.TRUE;
		else if(text.trim().equalsIgnoreCase(Boolean.FALSE.toString()))
			return Boolean.FALSE;
		else
			return defaultValue;
	}

	protected int readIntegerAttribute(Attributes attributes, String attributeName, int defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return Integer.parseInt(text.trim());
	}

	protected float readFloatAttribute(Attributes attributes, String attributeName, float defaultValue)
	{
		String text = attributes.getValue(attributeName);
		if(text == null || text.isEmpty())
			return defaultValue;
		else
			return Float.parseFloat(text.trim());
	}

}
