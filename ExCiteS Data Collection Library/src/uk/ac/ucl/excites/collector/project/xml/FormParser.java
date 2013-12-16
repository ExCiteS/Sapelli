/**
 * 
 */
package uk.ac.ucl.excites.collector.project.xml;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.ButtonField;
import uk.ac.ucl.excites.collector.project.model.CancelField;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.LabelField;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.MediaField;
import uk.ac.ucl.excites.collector.project.model.OrientationField;
import uk.ac.ucl.excites.collector.project.model.PhotoField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.model.Relationship;
import uk.ac.ucl.excites.collector.project.model.Field.Optionalness;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.util.xml.SubtreeParser;

/**
 * A {@link SubtreeParser} for <Form>s
 * 
 * @author mstevens
 */
public class FormParser extends SubtreeParser
{
	
	// STATICS--------------------------------------------------------
	
	//TAGS
	static private final String TAG_FORM = "Form";
	static private final String TAG_CHOICE = "Choice";
	static private final String TAG_AUDIO = "Audio";
	static private final String TAG_PHOTO = "Photo";
	static private final String TAG_ORIENTATION = "Orientation";
	static private final String TAG_BELONGS_TO = "BelongsTo";
	static private final String TAG_LINKS_TO = "LinksTo";
	static private final String TAG_BUTTON = "ButtonField";
	static private final String TAG_LABEL = "LabelField";
	
	//ATTRIBUTES
	static private final String ATTRIBUTE_FORM_NAME = "name";
	static private final String ATTRIBUTE_FORM_ID = "id";
	static private final String ATTRIBUTE_FORM_SCHEMA_ID = Schema.V1X_ATTRIBUTE_SCHEMA_ID;
	static private final String ATTRIBUTE_FORM_SCHEMA_VERSION = Schema.V1X_ATTRIBUTE_SCHEMA_VERSION;
	static private final String ATTRIBUTE_FORM_STORE_END_TIME = "storeEndTime";
	static private final String ATTRIBUTE_FORM_START_FIELD = "startField";
	static private final String ATTRIBUTE_FORM_END_SOUND = "endSound";
	static private final String ATTRIBUTE_FORM_END_VIBRATE = "endVibrate";
	static private final String ATTRIBUTE_FORM_FORWARD_BUTTON_IMG = "forwardButtonImg";
	static private final String ATTRIBUTE_FORM_CANCEL_BUTTON_IMG = "cancelButtonImg";
	static private final String ATTRIBUTE_FORM_BACK_BUTTON_IMG = "backButtonImg";
	static private final String ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR = "buttonBackgroundColor";
	static private final String ATTRIBUTE_FORM_SHORTCUT_IMAGE = "shortcutImage";
	static private final String ATTRIBUTE_FORM_ANIMATION = "animation";
	static private final String ATTRIBUTE_FIELD_ID = "id";
	static private final String ATTRIBUTE_FIELD_JUMP = "jump";
	static private final String ATTRIBUTE_FIELD_NO_COLUMN = "noColumn";
	static private final String ATTRIBUTE_FIELD_LABEL = "label";
	static private final String ATTRIBUTE_FIELD_BACKGROUND_COLOR = "backgroundColor";
	static private final String ATTRIBUTE_FIELD_SKIP_ON_BACK = "skipOnBack";
	static private final String ATTRIBUTE_CHOICE_VALUE = "value";
	static private final String ATTRIBUTE_DISABLE_FIELD = "disableField";
	static private final String ATTRIBUTE_SHOW_FORWARD = "showForward";
	static private final String ATTRIBUTE_SHOW_CANCEL = "showCancel";
	static private final String ATTRIBUTE_SHOW_BACK = "showBack";
	static private final String ATTRIBUTE_RELATIONSHIP_FORM = "currentForm";
	static private final String ATTRIBUTE_LABEL_TEXT = "text";
	
	// DYNAMICS-------------------------------------------------------
	private ProjectParser projectParser;
	private Project project;
	private Form currentForm;
	private String formStartFieldId;
	private ChoiceField currentChoice;	
	private HashMap<Field, String> fieldToJumpId;
	private Hashtable<String, Field> idToField;
	private HashMap<MediaField, String> mediaAttachToDisableId;

	public FormParser(ProjectParser projectParser, Project project, ProjectParser.Format format)
	{
		super(projectParser, TAG_FORM);
		reset(); //!!!
		this.projectParser = projectParser;
		this.project = projectParser.getProject();
	}

	@Override
	public void reset()
	{
		currentForm = null;
		formStartFieldId = null;
		fieldToJumpId = new HashMap<Field, String>();
		idToField = new Hashtable<String, Field>();
		mediaAttachToDisableId = new HashMap<MediaField, String>();
	}
	
	@Override
	protected void parseStartElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		// <Form>
		if(qName.equals(TAG_FORM))
		{
			if(currentForm != null)
				throw new SAXException("Forms cannot be nested!");
			
			String id;
			if(projectParser.getFormat() == ProjectParser.Format.v1_x)
			{	//backwards compatibility
				id = readRequiredStringAttribute(TAG_FORM, attributes, ATTRIBUTE_FORM_NAME);
				if(project.getForms().isEmpty()) //only for 1st, and assumed only, currentForm
				{
					int schemaID = Integer.parseInt(readRequiredStringAttribute(TAG_FORM, attributes, ATTRIBUTE_FORM_SCHEMA_ID, "because this is a v1.x project"));
					int schemaVersion = readIntegerAttribute(attributes, ATTRIBUTE_FORM_SCHEMA_VERSION, Schema.V1X_DEFAULT_SCHEMA_VERSION);
					project.setSchema(schemaID, schemaVersion); //schemaID will be used as projectID
				}
			}
			else
				id = readRequiredStringAttribute(TAG_FORM, attributes, ATTRIBUTE_FORM_ID);
			currentForm = new Form(project, id); // the form will add itself to the project and take the next available index
			// Shortcut image:
			currentForm.setShortcutImageRelativePath(readStringAttribute(attributes, ATTRIBUTE_FORM_SHORTCUT_IMAGE, null));
			// Store end time?:
			currentForm.setStoreEndTime(readBooleanAttribute(attributes, ATTRIBUTE_FORM_STORE_END_TIME, Form.END_TIME_DEFAULT));
			// Sound end vibration at the end of the currentForm:
			// Get the sound path
			currentForm.setEndSoundRelativePath(readStringAttribute(attributes, ATTRIBUTE_FORM_END_SOUND, null));
			currentForm.setVibrateOnEnd(readBooleanAttribute(attributes, ATTRIBUTE_FORM_END_VIBRATE, Form.DEFAULT_VIBRATE));
			// Which buttons are allowed to show:
			currentForm.setShowBack(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_BACK, Form.DEFAULT_SHOW_BACK));
			currentForm.setShowCancel(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_CANCEL, Form.DEFAULT_SHOW_CANCEL));
			currentForm.setShowForward(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_FORWARD, Form.DEFAULT_SHOW_FORWARD));
			// Animation:
			currentForm.setAnimation(readBooleanAttribute(attributes, ATTRIBUTE_FORM_ANIMATION, Form.DEFAULT_ANIMATION));
			// ButtonField images:
			currentForm.setBackButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_BACK_BUTTON_IMG));
			currentForm.setCancelButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_CANCEL_BUTTON_IMG));
			currentForm.setForwardButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_FORWARD_BUTTON_IMG));
			// ButtonField background colour:
			currentForm.setButtonBackgroundColor(readStringAttribute(attributes, ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR, Form.DEFAULT_BUTTON_BACKGROUND_COLOR));
			// Start field:
			if(attributes.getValue(ATTRIBUTE_FORM_START_FIELD) != null && !attributes.getValue(ATTRIBUTE_FORM_START_FIELD).isEmpty())
				formStartFieldId = attributes.getValue(ATTRIBUTE_FORM_START_FIELD);
			else
				addWarning("No startField attribute, will use first field");
			
			//Activate this subtree parser:
			activate(); //!!!
		}
		// children of <Form> (fields)
		else if(currentForm != null)
		{
			// <Choice>
			if(qName.equals(TAG_CHOICE))
			{
				currentChoice = new ChoiceField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), attributes.getValue(ATTRIBUTE_CHOICE_VALUE), currentChoice); // old currentChoice becomes the parent (if it is null that's ok)
				newField(currentChoice, attributes);
				// No column:
				currentChoice.setNoColumn(readBooleanAttribute(attributes, ATTRIBUTE_FIELD_NO_COLUMN, Field.DEFAULT_NO_COLUMN));
				// Other attributes:
				if(attributes.getValue("img") != null)
					currentChoice.setImageRelativePath(attributes.getValue("img"));
				if(attributes.getValue("alt") != null)
					currentChoice.setAltText(attributes.getValue("alt"));
				currentChoice.setCols(readIntegerAttribute(attributes, "cols", ChoiceField.DEFAULT_NUM_COLS));
				currentChoice.setRows(readIntegerAttribute(attributes, "rows", ChoiceField.DEFAULT_NUM_ROWS));
				currentChoice.setCrossed(readBooleanAttribute(attributes, "crossed", ChoiceField.DEFAULT_CROSSED));
				currentChoice.setCrossColor(readStringAttribute(attributes, "crossColor", ChoiceField.DEFAULT_CROSS_COLOR));
			}
			// <Location>
			else if(qName.equals("Location"))
			{
				LocationField locField = new LocationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
				newField(locField, attributes);
				// Location type:
				String type = attributes.getValue("type");
				if("Any".equalsIgnoreCase(type))
					locField.setType(LocationField.TYPE_ANY);
				else if("GPS".equalsIgnoreCase(type))
					locField.setType(LocationField.TYPE_GPS);
				else if("Network".equalsIgnoreCase(type))
					locField.setType(LocationField.TYPE_GPS);
				else if(type != null) // unrecognised location type
					addWarning("Unknown Location type (" + type + ").");
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
				photoField.setCaptureButtonImageRelativePath(attributes.getValue("captureImg"));
				photoField.setApproveButtonImageRelativePath(attributes.getValue("approveImg"));
				photoField.setDiscardButtonImageRelativePath(attributes.getValue("discardImg"));
			}
			// <Audio>
			else if(qName.equals(TAG_AUDIO))
			{
				AudioField audioField = new AudioField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
				newField(audioField, attributes);
				mediaAttachmentAttributes(audioField, attributes);
				audioField.setStartRecImageRelativePath(attributes.getValue("startRecImg"));
				audioField.setStopRecImageRelativePath(attributes.getValue("stopRecImg"));
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
			// <BelongsTo>
			else if(qName.equals(TAG_BELONGS_TO))
			{
				Relationship belongsTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.MANY_TO_ONE);
				newField(belongsTo, attributes);
				projectParser.addRelationship(belongsTo, readRequiredStringAttribute(qName, attributes, ATTRIBUTE_RELATIONSHIP_FORM));
			}
			// <LinksTo>
			else if(qName.equals(TAG_LINKS_TO))
			{
				Relationship linksTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.LINK);
				newField(linksTo, attributes);
				projectParser.addRelationship(linksTo, readRequiredStringAttribute(qName, attributes, ATTRIBUTE_RELATIONSHIP_FORM));
			}
			// <ButtonField>
			else if(qName.equals(TAG_BUTTON))
			{
				ButtonField btn = new ButtonField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_BUTTON, attributes, ATTRIBUTE_FIELD_LABEL));
				newField(btn, attributes);
			}
			// <LabelField>
			else if(qName.equals(TAG_LABEL))
			{
				LabelField lbl = new LabelField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_LABEL, attributes, ATTRIBUTE_LABEL_TEXT));
				newField(lbl, attributes);
			}
			// Add future field types here...
			// TODO TextField, Checkbox, etc.
			// <?> in <Form>
			else
			{
				addWarning("Ignored unrecognised or invalidly placed element called \"" + qName + "\" occuring within <" + TAG_FORM + ">.");
			}
		}
		// <?> outside of <Form> (shouldn't happen)
		else
		{
			throw new IllegalArgumentException("FormParser only deals with elements that are equal to, or contained within <" + TAG_FORM + ">.");
		}
	}
	
	/**
	 * Adds field to current currentForm, sets optionalness, remembers id & jump
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
		
		// f.setSkipOnBack(readBooleanAttribute(attributes, ATTRIBUTE_FIELD_SKIP_ON_BACK, Field.DEFAULT_SKIP_ON_BACK)); //TODO skip on back?
		f.setBackgroundColor(readStringAttribute(attributes, ATTRIBUTE_FIELD_BACKGROUND_COLOR, Field.DEFAULT_BACKGROUND_COLOR));
		
		// Which buttons are allowed to show:
		f.setShowBack(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_BACK, Field.DEFAULT_SHOW_BACK));
		f.setShowCancel(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_CANCEL, Field.DEFAULT_SHOW_CANCEL));
		f.setShowForward(readBooleanAttribute(attributes, ATTRIBUTE_SHOW_FORWARD, Field.DEFAULT_SHOW_FORWARD));
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
	
	private void mediaAttachmentAttributes(MediaField ma, Attributes attributes)
	{
		setOptionalness(ma, attributes);
		ma.setMax(readIntegerAttribute(attributes, "max", MediaField.DEFAULT_MAX));
		if(attributes.getValue(ATTRIBUTE_DISABLE_FIELD) != null)
			mediaAttachToDisableId.put(ma, attributes.getValue(ATTRIBUTE_DISABLE_FIELD).trim());
	}
	
	private void rememberIDAndJump(Field f, Attributes attributes)
	{
		// Remember ID of field itself:
		if(f.getID() != null)
		{
			if(idToField.get(f.getID()) != null)
				addWarning("Duplicate field ID: " + f.getID() + " (possibly based on value)!");
			idToField.put(f.getID(), f);
		}
		
		// Remember intra-currentForm jumps:
		if(attributes.getValue(ATTRIBUTE_FIELD_JUMP) != null)
		{
			String jumpToId = attributes.getValue(ATTRIBUTE_FIELD_JUMP).trim();
			//make _END currentForm-specific:
			if(jumpToId.equalsIgnoreCase(EndField.ID))
				jumpToId = EndField.ID(f.getForm());
			//make _CANCEL currentForm-specific:
			if(jumpToId.equalsIgnoreCase(CancelField.ID))
				jumpToId = CancelField.ID(f.getForm());
			//Store field & jumpToId:
			fieldToJumpId.put(f, jumpToId);
		}
	}
	
	@Override
	protected void parseEndElement(String uri, String localName, String qName) throws SAXException
	{
		// </Choice>
		if(qName.equals(TAG_CHOICE))
		{
			if(currentChoice.isRoot() && currentChoice.isLeaf())
				throw new SAXException("Root choices need at least 1 child (but 2 children probably makes more sense).");
			currentChoice = currentChoice.getParent();
		}
		// </Form>
		else if(qName.equals(TAG_FORM))
		{
			// Resolve/set currentForm start field:
			Field startField = currentForm.getFields().get(0); // first field is the default start field
			if(formStartFieldId != null) // start field specified (by ID) in Form tag
			{
				Field specifiedStartField = currentForm.getField(formStartFieldId);
				if(specifiedStartField == null)
					addWarning("The specified start field (\"" + formStartFieldId + "\") of currentForm \"" + currentForm.getName() + "\" does not exist, using first field instead.");
				else
					startField = specifiedStartField;
			}
			currentForm.setStartField(startField);
			
			// Create an EndField and an CancelField instance such that _END and _CANCEL jumps can be resolved (these don't need to be added as actual fields)
			EndField endF = new EndField(currentForm);
			idToField.put(endF.getID(), endF);
			CancelField cancelF = new CancelField(currentForm);
			idToField.put(cancelF.getID(), cancelF);
			
			// Resolve jumps...
			for(Entry<Field, String> jump : fieldToJumpId.entrySet())
			{
				Field target = idToField.get(jump.getValue());
				if(target == null)
					addWarning("Cannot resolve jump ID " + jump.getValue());
				else
					jump.getKey().setJump(target);
			}
			
			// Resolve disabling of Choices by MediaAttachments...
			for(Entry<MediaField, String> disable : mediaAttachToDisableId.entrySet())
			{
				Field target = idToField.get(disable.getValue());
				if(target == null)
					addWarning("Cannot resolve disable field ID " + disable.getValue());
				else
					disable.getKey().setDisableChoice((ChoiceField) target);
			}
			
			// Deactivate this subtree parser:
			deactivate(); //will call reset() (+ warnings will be copied to owner)
		}
	}

	@Override
	protected boolean isSingleUse()
	{
		return false;
	}

}
