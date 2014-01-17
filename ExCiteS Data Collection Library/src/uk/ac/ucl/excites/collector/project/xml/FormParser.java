/**
 * 
 */
package uk.ac.ucl.excites.collector.project.xml;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.model.fields.AudioField;
import uk.ac.ucl.excites.collector.project.model.fields.ButtonField;
import uk.ac.ucl.excites.collector.project.model.fields.CancelField;
import uk.ac.ucl.excites.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.model.fields.Field.Optionalness;
import uk.ac.ucl.excites.collector.project.model.fields.LabelField;
import uk.ac.ucl.excites.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.collector.project.model.fields.MediaField;
import uk.ac.ucl.excites.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.collector.project.model.fields.Relationship;
import uk.ac.ucl.excites.collector.project.model.fields.lists.MultiListField;
import uk.ac.ucl.excites.collector.project.model.fields.lists.MultiListItem;
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
	static private final String TAG_BUTTON = "Button";
	static private final String TAG_LABEL = "Label";
	static private final String TAG_TEXTFIELD = "Text";
	static private final String TAG_CHECKBOX = "Check";
	static private final String TAG_LIST = "List";
	static private final String TAG_MULTILIST = "MultiList";
	static private final String TAG_LISTITEM = "Item";
	
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
	static private final String ATTRIBUTE_FIELD_LABELS = "labels";
	static private final String ATTRIBUTE_FIELD_BACKGROUND_COLOR = "backgroundColor";
	static private final String ATTRIBUTE_FIELD_SKIP_ON_BACK = "skipOnBack";
	static private final String ATTRIBUTE_FIELD_VALUE = "value";
	static private final String ATTRIBUTE_FIELD_DEFAULTVALUE = "defaultValue";
	static private final String ATTRIBUTE_FIELD_INITVALUE = "initValue";
	static private final String ATTRIBUTE_DISABLE_FIELD = "disableField";
	static private final String ATTRIBUTE_SHOW_FORWARD = "showForward";
	static private final String ATTRIBUTE_SHOW_CANCEL = "showCancel";
	static private final String ATTRIBUTE_SHOW_BACK = "showBack";
	static private final String ATTRIBUTE_RELATIONSHIP_FORM = "currentForm";
	static private final String ATTRIBUTE_LABEL_TEXT = "text";
	static private final String ATTRIBUTE_TEXT_MINLENGTH = "minLength";
	static private final String ATTRIBUTE_TEXT_MAXLENGTH = "maxLength";
	static private final String ATTRIBUTE_TEXT_MULTILINE = "multiLine";
	static private final String ATTRIBUTE_LIST_PRESELECT = "preSelectDefault";
	static private final String ATTRIBUTE_LISTITEM_DEFAULT = "default";
	
	
	// DYNAMICS-------------------------------------------------------
	private Project project;
	private Form currentForm;
	private String formStartFieldId;
	private ChoiceField currentChoice;
	private MultiListItem currentListItem;
	private HashMap<Field, String> fieldToJumpId;
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
			if(((ProjectParser) owner).getFormat() == ProjectParser.Format.v1_x)
			{	//backwards compatibility
				id = readRequiredStringAttribute(TAG_FORM, ATTRIBUTE_FORM_NAME, attributes);
				if(project.getForms().isEmpty()) //only for 1st, and assumed only, currentForm
				{
					int schemaID = Integer.parseInt(readRequiredStringAttribute(TAG_FORM, ATTRIBUTE_FORM_SCHEMA_ID, "because this is a v1.x project", attributes));
					int schemaVersion = readIntegerAttribute(ATTRIBUTE_FORM_SCHEMA_VERSION, Schema.V1X_DEFAULT_SCHEMA_VERSION, attributes);
					project.setSchema(schemaID, schemaVersion); //schemaID will be used as projectID
				}
			}
			else
				id = readRequiredStringAttribute(TAG_FORM, ATTRIBUTE_FORM_ID, attributes);
			currentForm = new Form(project, id); // the form will add itself to the project and take the next available index
			// Shortcut image:
			currentForm.setShortcutImageRelativePath(readStringAttribute(ATTRIBUTE_FORM_SHORTCUT_IMAGE, null, attributes));
			// Store end time?:
			currentForm.setStoreEndTime(readBooleanAttribute(ATTRIBUTE_FORM_STORE_END_TIME, Form.END_TIME_DEFAULT, attributes));
			// Sound end vibration at the end of the currentForm:
			// Get the sound path
			currentForm.setEndSoundRelativePath(readStringAttribute(ATTRIBUTE_FORM_END_SOUND, null, attributes));
			currentForm.setVibrateOnEnd(readBooleanAttribute(ATTRIBUTE_FORM_END_VIBRATE, Form.DEFAULT_VIBRATE, attributes));
			// Which buttons are allowed to show:
			currentForm.setShowBack(readBooleanAttribute(ATTRIBUTE_SHOW_BACK, Form.DEFAULT_SHOW_BACK, attributes));
			currentForm.setShowCancel(readBooleanAttribute(ATTRIBUTE_SHOW_CANCEL, Form.DEFAULT_SHOW_CANCEL, attributes));
			currentForm.setShowForward(readBooleanAttribute(ATTRIBUTE_SHOW_FORWARD, Form.DEFAULT_SHOW_FORWARD, attributes));
			// Animation:
			currentForm.setAnimation(readBooleanAttribute(ATTRIBUTE_FORM_ANIMATION, Form.DEFAULT_ANIMATION, attributes));
			// ButtonField images:
			currentForm.setBackButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_BACK_BUTTON_IMG));
			currentForm.setCancelButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_CANCEL_BUTTON_IMG));
			currentForm.setForwardButtonImageRelativePath(attributes.getValue(ATTRIBUTE_FORM_FORWARD_BUTTON_IMG));
			// ButtonField background colour:
			currentForm.setButtonBackgroundColor(readStringAttribute(ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR, Form.DEFAULT_BUTTON_BACKGROUND_COLOR, attributes));
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
				currentChoice = new ChoiceField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), attributes.getValue(ATTRIBUTE_FIELD_VALUE), currentChoice); // old currentChoice becomes the parent (if it is null that's ok)
				newField(currentChoice, attributes);
				// No column:
				currentChoice.setNoColumn(readBooleanAttribute(ATTRIBUTE_FIELD_NO_COLUMN, Field.DEFAULT_NO_COLUMN, attributes));
				// Other attributes:
				if(attributes.getValue("img") != null)
					currentChoice.setImageRelativePath(attributes.getValue("img"));
				if(attributes.getValue("alt") != null)
					currentChoice.setAltText(attributes.getValue("alt"));
				currentChoice.setCols(readIntegerAttribute("cols", ChoiceField.DEFAULT_NUM_COLS, attributes));
				currentChoice.setRows(readIntegerAttribute("rows", ChoiceField.DEFAULT_NUM_ROWS, attributes));
				currentChoice.setCrossed(readBooleanAttribute("crossed", ChoiceField.DEFAULT_CROSSED, attributes));
				currentChoice.setCrossColor(readStringAttribute("crossColor", ChoiceField.DEFAULT_CROSS_COLOR, attributes));
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
				orField.setStoreAzimuth(readBooleanAttribute("storeAzimuth", OrientationField.DEFAULT_STORE_AZIMUTH, attributes));
				orField.setStoreAzimuth(readBooleanAttribute("storePitch", OrientationField.DEFAULT_STORE_PITCH, attributes));
				orField.setStoreAzimuth(readBooleanAttribute("storeRoll", OrientationField.DEFAULT_STORE_ROLL, attributes));
			}
			// <BelongsTo>
			else if(qName.equals(TAG_BELONGS_TO))
			{
				Relationship belongsTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.MANY_TO_ONE);
				newField(belongsTo, attributes);
				((ProjectParser) owner).addRelationship(belongsTo, readRequiredStringAttribute(qName, ATTRIBUTE_RELATIONSHIP_FORM, attributes));
			}
			// <LinksTo>
			else if(qName.equals(TAG_LINKS_TO))
			{
				Relationship linksTo = new Relationship(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), Relationship.Type.LINK);
				newField(linksTo, attributes);
				((ProjectParser) owner).addRelationship(linksTo, readRequiredStringAttribute(qName, ATTRIBUTE_RELATIONSHIP_FORM, attributes));
			}
			// <Button>
			else if(qName.equals(TAG_BUTTON))
			{
				ButtonField btn = new ButtonField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_BUTTON, ATTRIBUTE_FIELD_LABEL, attributes));
				newField(btn, attributes);
			}
			// <Label>
			else if(qName.equals(TAG_LABEL))
			{
				LabelField lbl = new LabelField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_LABEL, ATTRIBUTE_LABEL_TEXT, attributes));
				newField(lbl, attributes);
			}
			// <Textbox>
			else if(qName.equals(TAG_TEXTFIELD))
			{
				EditTextField txtField = new EditTextField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_TEXTFIELD, attributes, ATTRIBUTE_FIELD_LABEL));
				txtField.setMinLength(readIntegerAttribute(ATTRIBUTE_TEXT_MINLENGTH, EditTextField.DEFAULT_MIN_LENGTH, attributes));
				txtField.setMaxLength(readIntegerAttribute(ATTRIBUTE_TEXT_MAXLENGTH, EditTextField.DEFAULT_MAX_LENGTH, attributes));
				txtField.setMultiline(readBooleanAttribute(ATTRIBUTE_TEXT_MULTILINE, EditTextField.DEFAULT_MULTILINE, attributes));
				txtField.setInitialValue(readStringAttribute(EditTextField.DEFAULT_VALUE, attributes, ATTRIBUTE_FIELD_DEFAULTVALUE, ATTRIBUTE_FIELD_INITVALUE));
				newField(txtField, attributes);
			}
			// <Checkbox>
			else if(qName.equals(TAG_CHECKBOX))
			{
				CheckBoxField chbxField = new CheckBoxField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_CHECKBOX, attributes, ATTRIBUTE_FIELD_LABEL));
				chbxField.setValue(readBooleanAttribute(ATTRIBUTE_FIELD_DEFAULTVALUE, CheckBoxField.DEFAULT_VALUE, attributes));
				newField(chbxField, attributes);
			}
			// <List> or <MultiList> (these are in fact just synonyms, but we added both to avoid confusing novice form designers with terminoly that refers to a multi-level list when they only need a flat list)  
			else if(qName.equals(TAG_LIST) || qName.equals(TAG_MULTILIST))
			{
				MultiListField ml = new MultiListField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readRequiredStringAttribute(TAG_LABEL, attributes, ATTRIBUTE_FIELD_LABELS, ATTRIBUTE_FIELD_LABEL));
				ml.setPreSelect(readBooleanAttribute(ATTRIBUTE_LIST_PRESELECT, MultiListField.DEFAULT_PRESELECT, attributes));
				newField(ml, attributes);
				currentListItem = ml.getItemsRoot();
			}
			// <Item> (contained within <List> or <MultiList>, and maybe other things later)
			else if(qName.equals(TAG_LISTITEM))
			{
				if(currentListItem != null)
				{
					currentListItem = new MultiListItem(currentListItem, readRequiredStringAttribute(TAG_LISTITEM, ATTRIBUTE_FIELD_VALUE, attributes));
					if(readBooleanAttribute(ATTRIBUTE_LISTITEM_DEFAULT, false, attributes))
					{
						if(currentListItem.getParent().getDefaultChild() == null)
							currentListItem.getParent().setDefaultChild(currentListItem);
						else
							addWarning("More than 1 item marked as default within one of the (sub)lists of MultiListField " + currentListItem.getField().getID() + ", using 1st item marked as defaut as the default for the list.");
					}
				}
				//else if(otherListItemContainingField != null) { /* ... */ }
				else
					addWarning("Ignored <" + TAG_LISTITEM + "> element occuring outside <" + TAG_LIST + "> or  <" + TAG_MULTILIST + ">.");
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
		f.setBackgroundColor(readStringAttribute(ATTRIBUTE_FIELD_BACKGROUND_COLOR, Field.DEFAULT_BACKGROUND_COLOR, attributes));
		
		// Which buttons are allowed to show:
		f.setShowBack(readBooleanAttribute(ATTRIBUTE_SHOW_BACK, Field.DEFAULT_SHOW_BACK, attributes));
		f.setShowCancel(readBooleanAttribute(ATTRIBUTE_SHOW_CANCEL, Field.DEFAULT_SHOW_CANCEL, attributes));
		f.setShowForward(readBooleanAttribute(ATTRIBUTE_SHOW_FORWARD, Field.DEFAULT_SHOW_FORWARD, attributes));
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
		ma.setMax(readIntegerAttribute("max", MediaField.DEFAULT_MAX, attributes));
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
