/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.xml;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.JumpSource;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField.ButtonColumnType;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.xml.SubtreeParser;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;

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
	static private final String TAG_LOCATION = "Location";
	static private final String TAG_ORIENTATION = "Orientation";
	static private final String TAG_BELONGS_TO = "BelongsTo";
	static private final String TAG_LINKS_TO = "LinksTo";
	static private final String TAG_CONSTRAINT = "Constraint";
	static private final String TAG_BUTTON = "Button";
	static private final String TAG_LABEL = "Label";
	static private final String TAG_TEXTFIELD = "Text";
	static private final String TAG_CHECKBOX = "Check";
	static private final String TAG_LIST = "List";
	static private final String TAG_MULTILIST = "MultiList";
	static private final String TAG_LISTITEM = "Item";
	static private final String TAG_PAGE = "Page";
	static private final String TAG_TRIGGER = "Trigger";
	
	//ATTRIBUTES
	static private final String ATTRIBUTE_FORM_NAME = "name";
	static private final String ATTRIBUTE_FORM_ID = "id";
	static private final String ATTRIBUTE_FORM_SCHEMA_ID = Schema.V1X_ATTRIBUTE_SCHEMA_ID;
	static private final String ATTRIBUTE_FORM_SCHEMA_VERSION = Schema.V1X_ATTRIBUTE_SCHEMA_VERSION;
	static private final String ATTRIBUTE_FORM_STORE_END_TIME = "storeEndTime";
	static private final String ATTRIBUTE_FORM_START_FIELD = "startField";
	static private final String ATTRIBUTE_FORM_END = "end"; // 1.x compatibility
	static private final String ATTRIBUTE_FORM_NEXT = "next";
	static private final String ATTRIBUTE_FORM_END_SOUND = "endSound"; // 1.x compatibility
	static private final String ATTRIBUTE_FORM_SAVE_SOUND = "saveSound";
	static private final String ATTRIBUTE_FORM_END_VIBRATE = "endVibrate"; // 1.x compatibility
	static private final String ATTRIBUTE_FORM_SAVE_VIBRATE = "saveVibrate";
	static private final String ATTRIBUTE_FORM_FORWARD_BUTTON_IMG = "forwardButtonImg";
	static private final String ATTRIBUTE_FORM_CANCEL_BUTTON_IMG = "cancelButtonImg";
	static private final String ATTRIBUTE_FORM_BACK_BUTTON_IMG = "backButtonImg";
	static private final String ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR = "buttonBackgroundColor";
	static private final String ATTRIBUTE_FORM_SHORTCUT_IMAGE = "shortcutImage";
	static private final String ATTRIBUTE_FORM_ANIMATION = "animation";
	static private final String ATTRIBUTE_FORM_OBFUSCATE_MEDIA_FILES = "obfuscateMediaFiles";
	static private final String ATTRIBUTE_FORM_SINGLE_PAGE = "singlePage";
	static private final String ATTRIBUTE_SKIP_ON_BACK = "skipOnBack"; // used on both FORM and FIELD
	static private final String ATTRIBUTE_FIELD_ID = "id";
	static private final String ATTRIBUTE_FIELD_JUMP = "jump";
	static private final String ATTRIBUTE_FIELD_OPTIONAL = "optional";
	static private final String ATTRIBUTE_FIELD_NO_COLUMN = "noColumn";
	static private final String ATTRIBUTE_FIELD_ALT = "alt";
	static private final String ATTRIBUTE_FIELD_IMG = "img";
	static private final String ATTRIBUTE_FIELD_LABEL = "label";
	static private final String ATTRIBUTE_FIELD_LABELS = "labels";
	static private final String ATTRIBUTE_FIELD_BACKGROUND_COLOR = "backgroundColor";
	static private final String ATTRIBUTE_FIELD_SHOW_ON_CREATE = "showOnCreate";
	static private final String ATTRIBUTE_FIELD_SHOW_ON_EDIT = "showOnEdit";
	static private final String ATTRIBUTE_FIELD_VALUE = "value";
	static private final String ATTRIBUTE_FIELD_DEFAULTVALUE = "defaultValue";
	static private final String ATTRIBUTE_FIELD_INITVALUE = "initValue";
	static private final String ATTRIBUTE_DISABLE_FIELD = "disableField";
	static private final String ATTRIBUTE_SHOW_FORWARD = "showForward";
	static private final String ATTRIBUTE_SHOW_CANCEL = "showCancel";
	static private final String ATTRIBUTE_SHOW_BACK = "showBack";
	static private final String ATTRIBUTE_CHOICE_ROWS = "rows";
	static private final String ATTRIBUTE_CHOICE_COLS = "cols";
	static private final String ATTRIBUTE_RELATIONSHIP_FORM = "form";
	static private final String ATTRIBUTE_RELATIONSHIP_HOLD = "hold";
	static private final String ATTRIBUTE_CONSTRAINT_COLUMN = "column";
	static private final String ATTRIBUTE_LABEL_TEXT = "text";
	static private final String ATTRIBUTE_TEXT_MINLENGTH = "minLength";
	static private final String ATTRIBUTE_TEXT_MAXLENGTH = "maxLength";
	static private final String ATTRIBUTE_TEXT_MULTILINE = "multiLine";
	static private final String ATTRIBUTE_LABEL_SCALE = "scale";
	static private final String ATTRIBUTE_LABEL_CENTERED = "centered";
	static private final String ATTRIBUTE_LIST_PRESELECT = "preSelectDefault";
	static private final String ATTRIBUTE_LISTITEM_DEFAULT = "default";
	static private final String ATTRIBUTE_BUTTON_COLUMN = "column";
	static private final String ATTRIBUTE_TRIGGER_KEY = "key";
	static private final String ATTRIBUTE_TRIGGER_KEYS = "keys";
	static private final String ATTRIBUTE_TRIGGER_FIXED_TIMER = "fixedTimer";
	static private final String ATTRIBUTE_TRIGGER_JUMP = "jump";
	
	// DYNAMICS-------------------------------------------------------
	private Project project;
	private Form currentForm;
	private String formStartFieldId;
	
	private Boolean v1xFormShowBack = null;
	private Boolean v1xFormShowCancel = null;
	private Boolean v1xFormShowForward = null;
	
	private ChoiceField currentChoice;
	private MultiListItem currentListItem;
	private Relationship currentRelationship;
	private Page currentPage;
	
	private HashMap<JumpSource, String> jumpSourceToJumpTargetId;
	private Hashtable<String, Field> idToField;
	private HashMap<MediaField, String> mediaAttachToDisableId;

	public FormParser(ProjectParser projectParser)
	{
		super(projectParser, TAG_FORM);
		reset(); //!!!
		this.project = projectParser.getProject();
	}

	@Override
	public void reset()
	{
		currentForm = null;
		formStartFieldId = null;
		jumpSourceToJumpTargetId = new HashMap<JumpSource, String>();
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
			
			String id = readRequiredStringAttribute(TAG_FORM, attributes, true, false, ATTRIBUTE_FORM_ID, ATTRIBUTE_FORM_NAME); // "name" is v1.x syntax but still accepted in v2.0 (yet "id" is preferred)
			ProjectParser.Format format = ((ProjectParser) owner).getFormat();
			if(format == ProjectParser.Format.v1_x)
			{	// Backwards compatibility
				if(project.getForms().isEmpty()) // only for 1st, and assumed only, currentForm
				{
					int schemaID = readRequiredIntegerAttribute(TAG_FORM, ATTRIBUTE_FORM_SCHEMA_ID, "because this is a v1.x project", attributes);
					int schemaVersion = readIntegerAttribute(ATTRIBUTE_FORM_SCHEMA_VERSION, Schema.V1X_DEFAULT_SCHEMA_VERSION, attributes);
					project.setSchema(schemaID, schemaVersion); //schemaID will be used as projectID
				}
				else
					throw new SAXException("Only single-Form v1.x projects are supported");
			}
			currentForm = new Form(project, id); // the form will add itself to the project and take the next available form index
			// Shortcut image:
			currentForm.setShortcutImageRelativePath(readStringAttribute(ATTRIBUTE_FORM_SHORTCUT_IMAGE, null, attributes, false, false));
			// Next/end:
			try
			{
				currentForm.setNext(readStringAttribute(Form.DEFAULT_NEXT.name(), attributes, true, false, ATTRIBUTE_FORM_NEXT, ATTRIBUTE_FORM_END));
			}
			catch(IllegalArgumentException iae)
			{
				throw new SAXException("Invalid '" + ATTRIBUTE_FORM_NEXT + "' attribute value on <" + TAG_FORM + ">.", iae);
			}
			// Store end time?:
			currentForm.setStoreEndTime(readBooleanAttribute(ATTRIBUTE_FORM_STORE_END_TIME, Form.END_TIME_DEFAULT, attributes));
			// Sound end vibration at the end of the currentForm:
			currentForm.setSaveSoundRelativePath(readStringAttribute(null, attributes, false, false, ATTRIBUTE_FORM_SAVE_SOUND, ATTRIBUTE_FORM_END_SOUND)); // Get the sound path
			currentForm.setVibrateOnSave(readBooleanAttribute(Form.DEFAULT_VIBRATE, attributes, ATTRIBUTE_FORM_SAVE_VIBRATE, ATTRIBUTE_FORM_END_VIBRATE));
			// Which buttons are allowed to show (deprecated in format >= 2):
			if(attributes.getIndex(ATTRIBUTE_SHOW_BACK) != -1 || attributes.getIndex(ATTRIBUTE_SHOW_CANCEL) != -1 || attributes.getIndex(ATTRIBUTE_SHOW_FORWARD) != -1)
			{
				if(format == ProjectParser.Format.v1_x)
				{
					v1xFormShowBack = readBooleanAttribute(ATTRIBUTE_SHOW_BACK, Form.V1X_DEFAULT_SHOW_BACK, attributes);
					v1xFormShowCancel = readBooleanAttribute(ATTRIBUTE_SHOW_CANCEL, Form.V1X_DEFAULT_SHOW_CANCEL, attributes);
					v1xFormShowForward = readBooleanAttribute(ATTRIBUTE_SHOW_FORWARD, Form.V1X_DEFAULT_SHOW_FORWARD, attributes);
				}
				else
					addWarning("Attributes '" + ATTRIBUTE_SHOW_BACK + "', '" + ATTRIBUTE_SHOW_CANCEL + "' & '" + ATTRIBUTE_SHOW_FORWARD + "' are deprecated on <Form> in format >= 2.");
			}
			// Animation:
			currentForm.setAnimation(readBooleanAttribute(ATTRIBUTE_FORM_ANIMATION, Form.DEFAULT_ANIMATION, attributes));
			// Obfuscate Media Files:
			currentForm.setObfuscateMediaFiles(readBooleanAttribute(ATTRIBUTE_FORM_OBFUSCATE_MEDIA_FILES, Form.DEFAULT_OBFUSCATE_MEDIA_FILES, attributes));
			// Control button images:
			currentForm.setBackButtonImageRelativePath(readStringAttribute(ATTRIBUTE_FORM_BACK_BUTTON_IMG, null, attributes, false, false));
			currentForm.setCancelButtonImageRelativePath(readStringAttribute(ATTRIBUTE_FORM_CANCEL_BUTTON_IMG, null, attributes, false, false));
			currentForm.setForwardButtonImageRelativePath(readStringAttribute(ATTRIBUTE_FORM_FORWARD_BUTTON_IMG, null, attributes, false, false));
			// ButtonField background colour:
			currentForm.setButtonBackgroundColor(readStringAttribute(ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR, Form.DEFAULT_BUTTON_BACKGROUND_COLOR, attributes, true, false));
			// Single page form (all fields will be added to a single page):
			if(readBooleanAttribute(Form.DEFAULT_SINGLE_PAGE, attributes, ATTRIBUTE_FORM_SINGLE_PAGE))
				newPage(null);
			// Start field:
			formStartFieldId = readStringAttribute(ATTRIBUTE_FORM_START_FIELD, null, attributes, true, false);
			// skipOnBack:
			currentForm.setSkipOnBack(readBooleanAttribute(ATTRIBUTE_SKIP_ON_BACK, Form.DEFAULT_SKIP_ON_BACK, attributes));
			
			//Activate this subtree parser:
			activate(); //!!!
		}
		
		// Within a form...
		else if(currentForm != null)
		{	
			// Children of <BelongsTo> or <LinksTo> (we put this first so we can deal with fields or triggers appearing within a relationship)
			if(currentRelationship != null)
			{
				if(qName.equals(TAG_CONSTRAINT))
				{
					String columnName = readRequiredStringAttribute(getRelationshipTag(currentRelationship), ATTRIBUTE_CONSTRAINT_COLUMN, attributes, true, false);
					
					// Comparison attribute name:
					String comparisonAttrib = null;
					for(String compStr : RuleConstraint.COMPARISON_STRINGS)
						if(attributes.getIndex(compStr) != -1)
						{
							comparisonAttrib = compStr;
							break;
						}
					if(comparisonAttrib == null)
					{
						addWarning("<" + TAG_CONSTRAINT + "> does not contain an comparison attribute (i.e. 1 of: " + StringUtils.join(RuleConstraint.COMPARISON_STRINGS, ", ") + ").");
						return;
					}
					
					((ProjectParser) owner).addRelationshipConstraint(	currentRelationship,
																		columnName,
																		comparisonAttrib,
																		readRequiredStringAttribute(getRelationshipTag(currentRelationship), comparisonAttrib, attributes, true, true));
				}
				// <?> in <BelongsTo> or <LinksTo>
				else
					addWarning("Ignored unrecognised or invalidly placed element <" + qName + "> occuring within <" + getRelationshipTag(currentRelationship) + ">.");
			}
			
			// Children of <Form> (fields & triggers)...			
			// <Choice>
			else if(qName.equals(TAG_CHOICE))
			{
				currentChoice = new ChoiceField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), attributes.getValue(ATTRIBUTE_FIELD_VALUE), currentChoice); // old currentChoice becomes the parent (if it is null that's ok)
				newField(currentChoice, attributes);
				// noColumn:
				currentChoice.setNoColumn(readBooleanAttribute(ATTRIBUTE_FIELD_NO_COLUMN, Field.DEFAULT_NO_COLUMN, attributes));
				// Other attributes:
				currentChoice.setImageRelativePath(readStringAttribute(ATTRIBUTE_FIELD_IMG, null, attributes, false, false));
				currentChoice.setAltText(readStringAttribute(ATTRIBUTE_FIELD_ALT, null, attributes, false, false));
				currentChoice.setCols(readIntegerAttribute(ATTRIBUTE_CHOICE_COLS, ChoiceField.DEFAULT_NUM_COLS, attributes));
				currentChoice.setRows(readIntegerAttribute(ATTRIBUTE_CHOICE_ROWS, ChoiceField.DEFAULT_NUM_ROWS, attributes));
				currentChoice.setCrossed(readBooleanAttribute("crossed", ChoiceField.DEFAULT_CROSSED, attributes));
				currentChoice.setCrossColor(readStringAttribute("crossColor", ChoiceField.DEFAULT_CROSS_COLOR, attributes, true, false));
			}
			// <Location>
			else if(qName.equals(TAG_LOCATION))
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
				locField.setStartWithForm(readBooleanAttribute("startWithForm", LocationField.DEFAULT_START_WITH_FORM, attributes));
				locField.setWaitAtField(readBooleanAttribute("waitAtField", LocationField.DEFAULT_WAIT_AT_FIELD, attributes));
				locField.setTimeoutS(readIntegerAttribute("timeout", LocationField.DEFAULT_TIMEOUT_S, attributes));
				locField.setMaxAgeS(readIntegerAttribute("maxAge", LocationField.DEFAULT_MAX_AGE_S, attributes));
				locField.setMaxAccuracyRadius(readFloatAttribute("maxAccuracyRadius", LocationField.DEFAULT_MAX_ACCURACY_RADIUS, attributes));
				locField.setUseBestNonQualifyingLocationAfterTimeout(readBooleanAttribute("useBestKnownLocationOnTimeout", LocationField.DEFAULT_USE_BEST_NON_QUALIFYING_LOCATION_AFTER_TIMEOUT, attributes));
				// Storage settings:
				locField.setDoublePrecision(readBooleanAttribute("doublePrecision", LocationField.DEFAULT_DOUBLE_PRECISION, attributes));
				locField.setStoreAltitude(readBooleanAttribute("storeAltitude", LocationField.DEFAULT_STORE_ALTITUDE, attributes));
				locField.setStoreBearing(readBooleanAttribute("storeBearing", LocationField.DEFAULT_STORE_BEARING, attributes));
				locField.setStoreSpeed(readBooleanAttribute("storeSpeed", LocationField.DEFAULT_STORE_SPEED, attributes));
				locField.setStoreAccuracy(readBooleanAttribute("storeAccuracy", LocationField.DEFAULT_STORE_ACCURACY, attributes));
				locField.setStoreProvider(readBooleanAttribute("storeProvider", LocationField.DEFAULT_STORE_PROVIDER, attributes));
			}
			// <Photo>
			else if(qName.equals(TAG_PHOTO))
			{
				PhotoField photoField = new PhotoField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
				newField(photoField, attributes);
				mediaAttachmentAttributes(photoField, attributes);
				photoField.setUseNativeApp(readBooleanAttribute("useNativeApp", PhotoField.DEFAULT_USE_NATIVE_APP, attributes));
				// Camera options (only used when useNativeApp=false):
				photoField.setUseFrontFacingCamera(readBooleanAttribute("useFrontCamera", PhotoField.DEFAULT_USE_FRONT_FACING_CAMERA, attributes));
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
				photoField.setCaptureButtonImageRelativePath(readStringAttribute("captureImg", null, attributes, false, false));
				photoField.setApproveButtonImageRelativePath(readStringAttribute("approveImg", null, attributes, false, false));
				photoField.setDiscardButtonImageRelativePath(readStringAttribute("discardImg", null, attributes, false, false));
			}
			// <Audio>
			else if(qName.equals(TAG_AUDIO))
			{
				AudioField audioField = new AudioField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
				newField(audioField, attributes);
				mediaAttachmentAttributes(audioField, attributes);
				audioField.setStartRecImageRelativePath(readStringAttribute("startRecImg", null, attributes, false, false));
				audioField.setStopRecImageRelativePath(readStringAttribute("stopRecImg", null, attributes, false, false));
			}
			// <Orientation>
			else if(qName.equals(TAG_ORIENTATION))
			{
				OrientationField orField = new OrientationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID));
				newField(orField, attributes);
				orField.setStoreAzimuth(readBooleanAttribute("storeAzimuth", OrientationField.DEFAULT_STORE_AZIMUTH, attributes));
				orField.setStoreAzimuth(readBooleanAttribute("storePitch", OrientationField.DEFAULT_STORE_PITCH, attributes));
				orField.setStoreAzimuth(readBooleanAttribute("storeRoll", OrientationField.DEFAULT_STORE_ROLL, attributes));
			}
			// <BelongsTo>
			else if(qName.equals(TAG_BELONGS_TO))
			{
				Relationship belongsTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.MANY_TO_ONE);
				newRelationship(belongsTo, attributes);
			}
			// <LinksTo>
			else if(qName.equals(TAG_LINKS_TO))
			{
				Relationship linksTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.LINK);
				newRelationship(linksTo, attributes);
			}
			// <Button>
			else if(qName.equals(TAG_BUTTON))
			{
				ButtonField btn = new ButtonField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_BUTTON, ATTRIBUTE_FIELD_LABEL, attributes, false, true));
				newField(btn, attributes);
				try
				{
					btn.setColumnType(readStringAttribute(ATTRIBUTE_BUTTON_COLUMN, ButtonField.DEFAULT_COLUMN.name(), attributes, true, false));
				}
				catch(IllegalArgumentException iae)
				{
					throw new SAXException("Invalid '" + ATTRIBUTE_BUTTON_COLUMN + "' attribute value on <" + TAG_BUTTON + ">.", iae);
				}
				if(btn.getColumnType() == ButtonColumnType.DATETIME && btn.getOptional() != Optionalness.ALWAYS)
					addWarning("Button \"" + btn.getID() + "\" has a DateTime column but is not optional, this means the button will *have* to be pressed.");
			}
			// <Label>
			else if(qName.equals(TAG_LABEL))
			{
				LabelField lbl = new LabelField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_LABEL, attributes, false, true, ATTRIBUTE_LABEL_TEXT, ATTRIBUTE_FIELD_LABEL));
				newField(lbl, attributes);
				lbl.setTextSizeScale(readFloatAttribute(ATTRIBUTE_LABEL_SCALE, LabelField.DEFAULT_TEXT_SIZE_SCALE, attributes));
				lbl.setCentered(readBooleanAttribute(ATTRIBUTE_LABEL_CENTERED, LabelField.DEFAULT_TEXT_CENTERED, attributes));
			}
			// <Textbox>
			else if(qName.equals(TAG_TEXTFIELD))
			{
				TextBoxField txtField = new TextBoxField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_TEXTFIELD, ATTRIBUTE_FIELD_LABEL, attributes, false, true));
				newField(txtField, attributes); // first set general things like optionality (needed for getDefaultMinLength() below).
				
				// Deal with minimum & maximum length:
				if(txtField.getOptional() != Optionalness.ALWAYS && attributes.getIndex(ATTRIBUTE_TEXT_MINLENGTH) == -1)
					addWarning("Text field \"" + txtField.getID() + "\" is non-optional but no minimal length is defined, therefore the minimum will be set to " + TextBoxField.DEFAULT_MIN_LENGTH_NON_OPTIONAL + " character(s). If this is not appropriate then please use the '" + ATTRIBUTE_TEXT_MINLENGTH + "' attribute to set the minimum length explicitly.");				
				txtField.setMinLength(readIntegerAttribute(ATTRIBUTE_TEXT_MINLENGTH, txtField.getDefaultMinLength(), attributes));
				txtField.setMaxLength(readIntegerAttribute(ATTRIBUTE_TEXT_MAXLENGTH, TextBoxField.DEFAULT_MAX_LENGTH, attributes));
				
				txtField.setMultiline(readBooleanAttribute(ATTRIBUTE_TEXT_MULTILINE, TextBoxField.DEFAULT_MULTILINE, attributes));
				txtField.setInitialValue(readStringAttribute(TextBoxField.DEFAULT_INITIAL_VALUE, attributes, false, true, ATTRIBUTE_FIELD_DEFAULTVALUE, ATTRIBUTE_FIELD_INITVALUE));
				
			}
			// <Checkbox>
			else if(qName.equals(TAG_CHECKBOX))
			{
				CheckBoxField chbxField = new CheckBoxField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_CHECKBOX, ATTRIBUTE_FIELD_LABEL, attributes, false, true));
				chbxField.setInitialValue(readBooleanAttribute(ATTRIBUTE_FIELD_DEFAULTVALUE, CheckBoxField.DEFAULT_INITIAL_VALUE, attributes));
				newField(chbxField, attributes);
			}
			// <List> or <MultiList> (these are in fact just synonyms, but we added both to avoid confusing novice form designers with terminoly that refers to a multi-level list when they only need a flat list)  
			else if(qName.equals(TAG_LIST) || qName.equals(TAG_MULTILIST))
			{
				MultiListField ml = new MultiListField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_LABEL, attributes, false, true, ATTRIBUTE_FIELD_LABELS, ATTRIBUTE_FIELD_LABEL));
				ml.setPreSelect(readBooleanAttribute(ATTRIBUTE_LIST_PRESELECT, MultiListField.DEFAULT_PRESELECT, attributes));
				newField(ml, attributes);
				currentListItem = ml.getItemsRoot();
			}
			// <Item> (contained within <List> or <MultiList>, and maybe other things later)
			else if(qName.equals(TAG_LISTITEM))
			{
				if(currentListItem != null)
				{
					currentListItem = new MultiListItem(currentListItem, readRequiredStringAttribute(TAG_LISTITEM, ATTRIBUTE_FIELD_VALUE, attributes, false, true));
					if(readBooleanAttribute(ATTRIBUTE_LISTITEM_DEFAULT, false, attributes))
					{
						if(currentListItem.getParent().getDefaultChild() == null)
							currentListItem.getParent().setDefaultChild(currentListItem);
						else
							addWarning("More than 1 item marked as default within one of the (sub)lists of MultiListField " + currentListItem.getField().getID() + ", using 1st item marked as default as the default for the list.");
					}
				}
				//else if(otherListItemContainingField != null) { /* ... */ }
				else
					addWarning("Ignored <" + TAG_LISTITEM + "> element occuring outside <" + TAG_LIST + "> or  <" + TAG_MULTILIST + ">.");
			}
			// <Page> (Field composite)
			else if(qName.equals(TAG_PAGE))
			{
				newPage(attributes);
			}
			// <Trigger>
			else if(qName.equals(TAG_TRIGGER))
			{
				Trigger trigger = new Trigger();
				
				// Parse the attributes
				String keys = readStringAttribute(null, attributes, true, false, ATTRIBUTE_TRIGGER_KEY, ATTRIBUTE_TRIGGER_KEYS);
				if(keys != null)
					for(String k : keys.split(Trigger.KEY_SEPARATOR))
					{
						try
						{
							trigger.addKey(Trigger.Key.valueOf(k.toUpperCase()));
						}
						catch(Exception e)
						{
							addWarning("Unrecognised Trigger key: " + k);
						}
					}
				trigger.setFixedTimer(readIntegerAttribute(ATTRIBUTE_TRIGGER_FIXED_TIMER, Trigger.NO_TIMEOUT, attributes));
				if(attributes.getValue(ATTRIBUTE_TRIGGER_JUMP) != null) // Remember jump (always "intra-Form")
					jumpSourceToJumpTargetId.put(trigger, attributes.getValue(ATTRIBUTE_TRIGGER_JUMP).trim().toUpperCase()); // upper cased, for insensitivity
				
				// Add the trigger to the current Page
				if(currentPage != null)
					currentPage.addTrigger(trigger);
				// else add the triggers to the Form
				else
					currentForm.addTrigger(trigger);
			}
			// Add future field types here...
			// <?> in <Form>
			else
			{
				addWarning("Ignored unrecognised or invalidly placed element <" + qName + "> occuring within <" + TAG_FORM + ">.");
			}
		}
		// <?> outside of <Form> (shouldn't happen)
		else
		{
			throw new IllegalArgumentException("FormParser only deals with elements that are equal to, or contained within <" + TAG_FORM + ">.");
		}
	}
	
	/**
	 * @param attributes	may be null for implicit pages (i.e. the one for a singlePage form)
	 * @throws SAXException
	 */
	private void newPage(Attributes attributes) throws SAXException
	{
		if(currentPage != null)
			throw new SAXException("Nested <Page> elements are not allowed.");
		Page newPage = new Page(currentForm,
								attributes == null ?
									currentForm.getID() + "_page" :
									readStringAttribute(currentForm.getID() + "_page_" + currentForm.getFields().size(), attributes, true, false, ATTRIBUTE_FIELD_ID));
		newField(newPage, attributes);
		currentPage = newPage; //!!! the newPage helper variable avoids that newField() adds the page to itselfs instead of the form!
	}
	
	private void newRelationship(Relationship relationship, Attributes attributes) throws SAXException
	{
		newField(relationship, attributes);
		// Remember form name (to resolved later):
		((ProjectParser) owner).addRelationship(relationship, readRequiredStringAttribute(getRelationshipTag(relationship), ATTRIBUTE_RELATIONSHIP_FORM, attributes, true, false));
		
		// Other attributes:
		relationship.setHoldForeignRecord(readBooleanAttribute(ATTRIBUTE_RELATIONSHIP_HOLD, Relationship.DEFAULT_HOLD_FOREIGN_RECORD, attributes));
		// TODO ? updateStartTimeUponLeave, saveBeforeFormChange, discardBeforeLeave (only for linksTo) ?
		
		// Remember relationship so constraints can be parsed for it:
		currentRelationship = relationship;
	}
	
	/**
	 * Adds field to current currentForm or currentPage, sets optionalness, remembers id & jump & reads various Field attributes
	 * 
	 * @param field		the Field object
	 * @param attributes	may be null for implicit fields (fields that are inserted by the parser but do not explicitly appear in the XML, e.g. the Page for a singlePage form) 
	 * @throws SAXException
	 */
	private void newField(Field field, Attributes attributes) throws SAXException
	{
		// Warn about IDs starting with '_': //TODO test if no invalid XML chars
		if(field.getID().startsWith("_"))
		{
			// For really stupid cases ;-):
			for(EndField ef : EndField.GetEndFields(currentForm))
				if(ef.getID().equals(field.getID()))
					throw new SAXException(field.getID() + " is a reserved ID, don't use it for user-defined fields.");
			addWarning("Please avoid field IDs starting with '_' (" + field.getID() + ")."); 
		}
		
		// If the field is a root field: add it to the form or page, remember its ID, and set its optionalness:
		if(field.isRoot())
		{
			if(currentPage == null)
			{	// field is top-level (directly contained within the form, and not in a page first)...
				currentForm.addField(field);
				// ... and therefore it can be jumped to, so remember its ID (upper cased, for case insensitivity):
				if(idToField.put(field.getID().toUpperCase(), field) != null)
					throw new SAXException("Duplicate field ID '" + field.getID() + "' in Form '" + currentForm.getID() + "'! (Note: field and form IDs are case insensitive)");
			}
			else
				// the field is contained by a page:
				currentPage.addField(field);
			
			// Set optionalness:
			if(attributes != null)
			{
				String optText = attributes.getValue(ATTRIBUTE_FIELD_OPTIONAL);
				Optionalness opt = currentPage == null ? Field.DEFAULT_OPTIONAL : currentPage.getOptional(); // use default optionalness or that of the containing page
				if(optText != null && !optText.trim().isEmpty())
				{	
					optText = optText.trim();
					if("always".equalsIgnoreCase(optText) || Boolean.TRUE.toString().equalsIgnoreCase(optText))
						opt = Optionalness.ALWAYS;
					else if("notIfReached".equalsIgnoreCase(optText))
						opt = Optionalness.NOT_IF_REACHED;
					else if("never".equalsIgnoreCase(optText) || Boolean.FALSE.toString().equalsIgnoreCase(optText))
						opt = Optionalness.NEVER;
				}
				field.setOptional(opt);				
			}
		}
		
		// Read various optional Field attributes: 
		if(attributes != null)
		{	
			// Remember jumps (always "intra-Form", and not leaving a page unless the field can do that):
			if(attributes.getValue(ATTRIBUTE_FIELD_JUMP) != null)
			{
				if(currentPage == null || field.canJumpFromPage())
					jumpSourceToJumpTargetId.put(field, attributes.getValue(ATTRIBUTE_FIELD_JUMP).trim().toUpperCase()); // upper cased, for case insensitivity
				else if(currentPage != null)
					addWarning("Field \"" + field.getID() + "\" tries to jump away from the page, but is not allowed.");
			}
			
			// Skip on back:
			field.setSkipOnBack(readBooleanAttribute(ATTRIBUTE_SKIP_ON_BACK, Field.DEFAULT_SKIP_ON_BACK, attributes));
			
			// Show on create/edit:
			field.setShowOnCreate(readBooleanAttribute(ATTRIBUTE_FIELD_SHOW_ON_CREATE, Field.DEFAULT_SHOW_ON_CREATE, attributes));
			field.setShowOnEdit(readBooleanAttribute(ATTRIBUTE_FIELD_SHOW_ON_EDIT, Field.DEFAULT_SHOW_ON_EDIT, attributes));
			
			// Background colour:
			field.setBackgroundColor(readStringAttribute(ATTRIBUTE_FIELD_BACKGROUND_COLOR, Field.DEFAULT_BACKGROUND_COLOR, attributes, true, false));
			
			// Which buttons are allowed to show (with backwards compatibility for v1.0 forms which may have shopBack/showCancel/showForward at the form level):
			field.setShowBack((v1xFormShowBack != null ? v1xFormShowBack : true) && readBooleanAttribute(ATTRIBUTE_SHOW_BACK, Field.DEFAULT_SHOW_BACK, attributes));
			field.setShowCancel((v1xFormShowCancel != null ? v1xFormShowCancel : true) && readBooleanAttribute(ATTRIBUTE_SHOW_CANCEL, Field.DEFAULT_SHOW_CANCEL, attributes));
			field.setShowForward((v1xFormShowForward != null ? v1xFormShowForward : true) && readBooleanAttribute(ATTRIBUTE_SHOW_FORWARD, Field.DEFAULT_SHOW_FORWARD, attributes));
		}
	}
	
	private void mediaAttachmentAttributes(MediaField ma, Attributes attributes)
	{
		ma.setMax(readIntegerAttribute("max", MediaField.DEFAULT_MAX, attributes));
		if(attributes.getValue(ATTRIBUTE_DISABLE_FIELD) != null)
			mediaAttachToDisableId.put(ma, attributes.getValue(ATTRIBUTE_DISABLE_FIELD).trim().toUpperCase()); // upper cased, for case insensitivity
	}
	
	protected void closePage()
	{
		/*
		 * The 'optional' attribute of a page is only used to inherit from by contained fields (see newField()),
		 * at runtime it doesn't have meaning in itself because whether or not a page can be skipped or left is
		 * to be decided based on the optionalness and acquired values of the contained fields.
		 * Because of this the optionalness of the page is reset to ALWAYS after all contained fields are parsed.
		 */
		if(currentPage != null)
			currentPage.setOptional(Optionalness.ALWAYS);
		currentPage = null;
	}
	
	@Override
	protected void parseEndElement(String uri, String localName, String qName) throws SAXException
	{
		// </Choice>
		if(qName.equals(TAG_CHOICE))
		{
			if(currentChoice.isRoot() && currentChoice.isLeaf())
				throw new SAXException("Root choices need at least 1 child (but 2 or more children probably makes more sense).");
			currentChoice = currentChoice.getParent(); // parent (possibly null in case of root) becomes currentChoice
		}
		// </Item>, </List> or </MultiList>
		else if(qName.equals(TAG_LISTITEM) || qName.equals(TAG_LIST) || qName.equals(TAG_MULTILIST))
		{
			if(currentListItem.isRoot() && currentListItem.isLeaf())
				throw new SAXException("A list needs at least 1 <Item> (but 2 or more probably makes more sense).");
			if(!currentListItem.isLeaf() && currentListItem.getDefaultChild() == null)
				currentListItem.setDefaultChild(currentListItem.getChildren().get(0)); // first child become default
			currentListItem = currentListItem.getParent(); // parent (possibly null in case of root) becomes currentListItem
		}
		// </Page>
		else if(qName.equals(TAG_PAGE))
		{
			closePage();
		}
		// </BelongsTo> or </LinksTo> 
		else if(qName.equals(TAG_BELONGS_TO) || qName.equals(TAG_LINKS_TO))
		{
			currentRelationship = null;
		}
		// </Form>
		else if(qName.equals(TAG_FORM))
		{
			// in case of a singePage form:
			closePage();
			
			// Resolve/set currentForm start field:
			Field startField = currentForm.getFields().get(0); // first field is the default start field
			if(formStartFieldId != null) // try with field specified by ID in <Form startField="..."> (may be null)
			{
				Field specifiedStartField = currentForm.getField(formStartFieldId); // uses equalsIgnoreCase()
				if(specifiedStartField == null) //TODO throw exception instead
					addWarning("The specified start field (\"" + formStartFieldId + "\") of currentForm \"" + currentForm.getName() + "\" does not exist, using first field instead.");
				else
					startField = specifiedStartField;
			}
			currentForm.setStartField(startField);		
			
			// Add EndField instances (these don't need to be added as actual fields)
			for(EndField endF : EndField.GetEndFields(currentForm))
				idToField.put(endF.getID().toUpperCase(), endF); // upper cased, for case insensitivity (they should already be upper case, but just in case...)
			
			// Resolve jumps...
			for(Entry<JumpSource, String> jump : jumpSourceToJumpTargetId.entrySet())
			{
				Field target = idToField.get(jump.getValue());
				if(target == null)
					addWarning("Cannot resolve jump ID '" + jump.getValue() +  "' (case insensitive).");
				else
					jump.getKey().setJump(target); // set jump pointer (to a field object)
			}
			
			// Resolve disabling of Choices by MediaAttachments...
			for(Entry<MediaField, String> disable : mediaAttachToDisableId.entrySet())
			{
				Field target = idToField.get(disable.getValue());
				if(target == null)
					addWarning("Cannot resolve disable field ID '" + disable.getValue() +  "' (case insensitive).");
				else
					disable.getKey().setDisableChoice((ChoiceField) target);
			}
			
			// Deactivate this subtree parser:
			deactivate(); //will call reset() (+ warnings will be copied to owner)
		}
	}
	
	private String getRelationshipTag(Relationship relationship)
	{
		switch(relationship.getType())
		{
			case MANY_TO_ONE : return TAG_LINKS_TO;
			case LINK : return TAG_LINKS_TO;
		}
		throw new IllegalArgumentException("Unsupported relationship type");
	}

	@Override
	protected boolean isSingleUse()
	{
		return false;
	}

}
