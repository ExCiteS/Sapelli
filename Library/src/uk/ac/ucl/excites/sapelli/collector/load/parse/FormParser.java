/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.load.parse;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;
import java.util.Stack;

import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
import uk.ac.ucl.excites.sapelli.collector.load.process.TTVSynthesisTask;
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Description;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.JumpSource;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.BelongsToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField.ButtonColumnType;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LinksToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.media.MediaHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.xml.SubtreeParser;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLAttributes;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;

/**
 * A {@link SubtreeParser} for <Form>s
 * 
 * @author mstevens
 */
public class FormParser extends SubtreeParser<ProjectParser>
{
	
	// STATICS--------------------------------------------------------
	
	//TAGS
	static private final String TAG_FORM = "Form";
	static private final String TAG_BACK = "Back";
	static private final String TAG_CANCEL = "Cancel";
	static private final String TAG_FORWARD = "Forward";
	static private final String TAG_CHOICE = "Choice";
	static private final String TAG_AUDIO = "Audio";
	static private final String TAG_PHOTO = "Photo";
	static private final String TAG_LOCATION = "Location";
	static private final String TAG_ORIENTATION = "Orientation";
	static public final String TAG_BELONGS_TO = "BelongsTo";
	static public final String TAG_LINKS_TO = "LinksTo";
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
	static private final String TAG_ARGUMENT = "Argument";
	
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
	static private final String ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR = "buttonBackgroundColor"; // 1.x compatibility
	static private final String ATTRIBUTE_FORM_CONTROL_BACKGROUND_COLOR = "controlBackgroundColor";
	static private final String ATTRIBUTE_FORM_SHORTCUT_IMAGE = "shortcutImage";
	static private final String ATTRIBUTE_FORM_CLICK_ANIMATION = "clickAnimation";
	static private final String ATTRIBUTE_FORM_ANIMATION = "animation"; // 1.x compatibility, the same as clickAnimation
	static private final String ATTRIBUTE_FORM_DEFAULT_LANGUAGE = "defaultLanguage";
	static private final String ATTRIBUTE_FORM_SCREEN_TRANSITION = "screenTransition";
	static private final String ATTRIBUTE_FORM_AUDIO_FEEDBACK = "audioFeedback";
	static private final String ATTRIBUTE_FORM_OBFUSCATE_MEDIA_FILES = "obfuscateMediaFiles";
	static private final String ATTRIBUTE_FORM_SINGLE_PAGE = "singlePage";
	static private final String ATTRIBUTE_FORM_SHOW_IMAGE_SIZES = "showImgSizes";
	static private final String ATTRIBUTE_SKIP_ON_BACK = "skipOnBack"; // used on both FORM and FIELD
	static private final String ATTRIBUTE_FIELD_ID = "id";
	static private final String ATTRIBUTE_FIELD_JUMP = "jump";
	static private final String ATTRIBUTE_FIELD_OPTIONAL = "optional";
	static private final String ATTRIBUTE_FIELD_NO_COLUMN = "noColumn";
	static private final String ATTRIBUTE_FIELD_EDITABLE = "editable";
	static private final String ATTRIBUTE_FIELD_IMG = "img";
	static private final String ATTRIBUTE_FIELD_DESC = "desc";
	static private final String ATTRIBUTE_FIELD_DESCRIPTION = "description";
	static private final String[] ATTRIBUTE_FIELD_DESC_DESCRIPTION = { ATTRIBUTE_FIELD_DESC, ATTRIBUTE_FIELD_DESCRIPTION };
	static private final String ATTRIBUTE_FIELD_CAPTION = "caption";
	static private final String ATTRIBUTE_FIELD_CAPTIONS = "captions";
	static private final String ATTRIBUTE_FIELD_LABEL = "label"; // synonym for caption
	static private final String ATTRIBUTE_FIELD_LABELS = "labels"; // synonym for captions
	static private final String[] ATTRIBUTE_FIELD_CAPTION_SINGULAR = { ATTRIBUTE_FIELD_CAPTION, ATTRIBUTE_FIELD_LABEL };
	static private final String[] ATTRIBUTE_FIELD_CAPTION_PLURAL = { ATTRIBUTE_FIELD_CAPTION, ATTRIBUTE_FIELD_CAPTIONS, ATTRIBUTE_FIELD_LABEL, ATTRIBUTE_FIELD_LABELS };
	static private final String ATTRIBUTE_FIELD_BACKGROUND_COLOR = "backgroundColor";
	static private final String ATTRIBUTE_FIELD_SHOW_ON_CREATE = "showOnCreate";
	static private final String ATTRIBUTE_FIELD_SHOW_ON_EDIT = "showOnEdit";
	static private final String ATTRIBUTE_FIELD_SHOW_FORWARD = "showForward";
	static private final String ATTRIBUTE_FIELD_SHOW_BACK_ON_CREATE = "showBackOnCreate";
	static private final String ATTRIBUTE_FIELD_SHOW_BACK_ON_EDIT = "showBackOnEdit";
	static private final String ATTRIBUTE_FIELD_SHOW_CANCEL = "showCancel";
	static private final String ATTRIBUTE_FIELD_SHOW_CANCEL_ON_CREATE = "showCancelOnCreate";
	static private final String ATTRIBUTE_FIELD_SHOW_CANCEL_ON_EDIT = "showCancelOnEdit";
	static private final String ATTRIBUTE_FIELD_SHOW_FORWARD_ON_CREATE = "showForwardOnCreate";
	static private final String ATTRIBUTE_FIELD_SHOW_FORWARD_ON_EDIT = "showForwardOnEdit";
	static private final String ATTRIBUTE_FIELD_SHOW_BACK = "showBack";
	static private final String ATTRIBUTE_FIELD_VALUE = "value";
	static private final String ATTRIBUTE_FIELD_DEFAULTVALUE = "defaultValue";
	static private final String ATTRIBUTE_FIELD_INITVALUE = "initialValue";
	static private final String ATTRIBUTE_DISABLE_FIELD = "disableField";
	static private final String ATTRIBUTE_CHOICE_CAPTION_HEIGHT = "captionHeight";
	static private final String ATTRIBUTE_CHOICE_MATCH_TEXT_SIZE = "matchTextSize";
	static private final String ATTRIBUTE_CHOICE_ALT = "alt";
	static private final String[] ATTRIBUTE_CHOICE_ANSWER_DESC_DESCRIPTION = { "answerDesc", "answerDescription" };
	static private final String[] ATTRIBUTE_CHOICE_QUESTION_DESC_DESCRIPTION = { ATTRIBUTE_FIELD_DESC, ATTRIBUTE_FIELD_DESCRIPTION, "questionDesc", "questionDescription" };
	static private final String ATTRIBUTE_CHOICE_CROSSED = "crossed";
	static private final String ATTRIBUTE_CHOICE_CROSS_COLOR = "crossColor";
	static private final String ATTRIBUTE_CHOICE_ROWS = "rows";
	static private final String ATTRIBUTE_CHOICE_COLS = "cols";
	static private final String ATTRIBUTE_LOCATION_START_WITH = "startWith";
	static private final String ATTRIBUTE_LOCATION_START_WITH_FORM = "startWithForm"; // deprecated in favour of attribute above
	static private final String ATTRIBUTE_RELATIONSHIP_FORM = "form";
	static private final String ATTRIBUTE_RELATIONSHIP_HOLD = "hold";
	static private final String ATTRIBUTE_RELATIONSHIP_REMEMBER = "remember";
	static private final String ATTRIBUTE_CONSTRAINT_COLUMN = "column";
	static private final String ATTRIBUTE_TEXT_MINLENGTH = "minLength";
	static private final String ATTRIBUTE_TEXT_MAXLENGTH = "maxLength";
	static private final String ATTRIBUTE_TEXT_MULTILINE = "multiLine";
	static private final String ATTRIBUTE_TEXT_CONTENT = "content";
	static private final String ATTRIBUTE_TEXT_REGEX = "regex";
	static private final String ATTRIBUTE_TEXT_CAPITALISATION = "autoCaps";
	static private final String ATTRIBUTE_LABEL_SCALE = "scale";
	static private final String ATTRIBUTE_LABEL_CENTERED = "centered";
	static private final String ATTRIBUTE_LIST_PRESELECT = "preSelectDefault";
	static private final String ATTRIBUTE_LISTITEM_DEFAULT = "default";
	static private final String ATTRIBUTE_BUTTON_COLUMN = "column";
	static private final String ATTRIBUTE_MEDIA_MAX = "max";
	static private final String ATTRIBUTE_TRIGGER_KEY = "key";
	static private final String ATTRIBUTE_TRIGGER_KEYS = "keys";
	static private final String ATTRIBUTE_TRIGGER_FIXED_TIMER = "fixedTimer";
	static private final String ATTRIBUTE_TRIGGER_JUMP = "jump";
	static private final String ATTRIBUTE_ARGUMENT_PARAM = "param";
	static private final String ATTRIBUTE_ARGUMENT_VALUE = "value";
	
	// DYNAMICS-------------------------------------------------------
	private Project project;
	private Form currentForm;
	private String formStartFieldId;
	
	private Boolean v1xFormShowBack = null;
	private Boolean v1xFormShowCancel = null;
	private Boolean v1xFormShowForward = null;
	
	private Stack<Field> openFields;
	private Trigger openTrigger;
	private MultiListItem currentListItem;
	
	private boolean[] parsedControls = new boolean[Control.Type.values().length];
	
	private HashMap<JumpSource, String> jumpSourceToJumpTargetId;
	private Hashtable<String, Field> idToField;
	private HashMap<MediaField, String> mediaAttachToDisableId;
	
	private boolean choiceParentHadCaptionHeightAttribute = false;

	public FormParser(ProjectParser projectParser)
	{
		super(projectParser, TAG_FORM);
		this.project = projectParser.getProject();
		this.openFields = new Stack<Field>();
		this.jumpSourceToJumpTargetId = new HashMap<JumpSource, String>();
		this.idToField = new Hashtable<String, Field>();
		this.mediaAttachToDisableId = new HashMap<MediaField, String>();
	}

	@Override
	public void reset()
	{
		currentForm = null;
		for(int c = 0; c < parsedControls.length; c++)
			parsedControls[c] = false;
		openFields.clear();
		openTrigger = null;
		currentListItem = null;
		formStartFieldId = null;
		jumpSourceToJumpTargetId.clear();
		idToField.clear();
		mediaAttachToDisableId.clear();
		v1xFormShowBack = null;
		v1xFormShowCancel = null;
		v1xFormShowForward = null;
		choiceParentHadCaptionHeightAttribute = false;
	}
	
	@Override
	protected void parseStartElement(String uri, String localName, String qName, XMLAttributes attributes) throws Exception
	{
		// <Form>
		if(qName.equals(TAG_FORM))
		{
			if(currentForm != null)
				throw new SAXException("Forms cannot be nested!");
			
			String id = attributes.getRequiredString(TAG_FORM, true, false, ATTRIBUTE_FORM_ID, ATTRIBUTE_FORM_NAME); // "name" is v1.x syntax but still accepted in v2.0 (yet "id" is preferred)
			ProjectParser.Format format = owner.getFormat();
			if(format == ProjectParser.Format.v1_x)
			{	// Backwards compatibility
				if(project.getForms().isEmpty()) // only for 1st, and assumed only, currentForm
				{
					int schemaID = attributes.getRequiredInteger(TAG_FORM, ATTRIBUTE_FORM_SCHEMA_ID, "because this is a v1.x project");
					int schemaVersion = attributes.getInteger(ATTRIBUTE_FORM_SCHEMA_VERSION, Schema.V1X_DEFAULT_SCHEMA_VERSION);
					project.setV1XSchemaInfo(schemaID, schemaVersion); //schemaID will be used as projectID
				}
				else
					throw new SAXException("Only single-Form v1.x projects are supported");
			}
			currentForm = new Form(project, id); // the form will add itself to the project and take the next available form position
			// Shortcut image:
			currentForm.setShortcutImageRelativePath(attributes.getString(ATTRIBUTE_FORM_SHORTCUT_IMAGE, null, false, false));
			// Next/end:
			try
			{
				currentForm.setNext(attributes.getString(Form.DEFAULT_NEXT.name(), true, false, ATTRIBUTE_FORM_NEXT, ATTRIBUTE_FORM_END));
			}
			catch(IllegalArgumentException iae)
			{
				throw new Exception("Invalid '" + ATTRIBUTE_FORM_NEXT + "' attribute value on <" + TAG_FORM + ">.", iae);
			}
			// Store end time?:
			currentForm.setStoreEndTime(attributes.getBoolean(ATTRIBUTE_FORM_STORE_END_TIME, Form.END_TIME_DEFAULT));
			// Sound end vibration at the end of the currentForm:
			currentForm.setSaveSoundRelativePath(attributes.getString(null, false, false, ATTRIBUTE_FORM_SAVE_SOUND, ATTRIBUTE_FORM_END_SOUND)); // Get the sound path
			currentForm.setVibrateOnSave(attributes.getBoolean(Form.DEFAULT_VIBRATE, ATTRIBUTE_FORM_SAVE_VIBRATE, ATTRIBUTE_FORM_END_VIBRATE));
			// Which buttons are allowed to show (deprecated in format >= 2):
			if(attributes.contains(ATTRIBUTE_FIELD_SHOW_BACK) || attributes.contains(ATTRIBUTE_FIELD_SHOW_CANCEL) || attributes.contains(ATTRIBUTE_FIELD_SHOW_FORWARD))
			{
				if(format == ProjectParser.Format.v1_x)
				{
					v1xFormShowBack = attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_BACK, Form.V1X_DEFAULT_SHOW_BACK);
					v1xFormShowCancel = attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_CANCEL, Form.V1X_DEFAULT_SHOW_CANCEL);
					v1xFormShowForward = attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_FORWARD, Form.V1X_DEFAULT_SHOW_FORWARD);
				}
				else
					addWarning("Attributes '" + ATTRIBUTE_FIELD_SHOW_BACK + "', '" + ATTRIBUTE_FIELD_SHOW_CANCEL + "' & '" + ATTRIBUTE_FIELD_SHOW_FORWARD + "' are deprecated on <Form> in format >= 2.");
			}
			// Click Animation:
			currentForm.setClickAnimation(attributes.getBoolean(Form.DEFAULT_CLICK_ANIMATION, ATTRIBUTE_FORM_CLICK_ANIMATION, ATTRIBUTE_FORM_ANIMATION));
			// Screen Transition:
			try
			{
				currentForm.setScreenTransition(attributes.getString(ATTRIBUTE_FORM_SCREEN_TRANSITION, Form.DEFAULT_SCREEN_TRANSITION.name(), true, false));
			}
			catch(IllegalArgumentException iae)
			{
				addWarning("Invalid '" + ATTRIBUTE_FORM_SCREEN_TRANSITION + "' attribute value on <" + TAG_FORM + ">. Default Screen Transition is going to be used.");
			}
			// Form language (for TTS synthesis) -- allow null so that we can fall back on project language:
			currentForm.setDefaultLanguage(attributes.getString(ATTRIBUTE_FORM_DEFAULT_LANGUAGE, null, true, false));
			// Add AudioFeedback:
			try
			{
				currentForm.setAudioFeedback(attributes.getString(ATTRIBUTE_FORM_AUDIO_FEEDBACK, Form.DEFAULT_AUDIO_FEEDBACK.name(), true, false));
				if(currentForm.getAudioFeedback() != null && currentForm.getAudioFeedback() != AudioFeedback.NONE)
					addWarning("Older Android devices may require SpeechSynthesis Data Installer to be installed for text-to-speech to work."); // TODO move this to synthesis?
			}
			catch(IllegalArgumentException iae)
			{
				addWarning("Invalid '" + ATTRIBUTE_FORM_AUDIO_FEEDBACK + "' attribute value on <" + TAG_FORM + ">. Default Audio Feedback is going to be used.");
			}
			// Obfuscate Media Files:
			currentForm.setObfuscateMediaFiles(attributes.getBoolean(ATTRIBUTE_FORM_OBFUSCATE_MEDIA_FILES, Form.DEFAULT_OBFUSCATE_MEDIA_FILES));
			
			// Control background colour:
			currentForm.setControlBackgroundColor(attributes.getString(Form.DEFAULT_CONTROL_BACKGROUND_COLOR, true, false, ATTRIBUTE_FORM_CONTROL_BACKGROUND_COLOR, ATTRIBUTE_FORM_BUTTON_BACKGROUND_COLOR));
			
			// Show image sizes:
			currentForm.setShowImageSizes(attributes.getBoolean(ATTRIBUTE_FORM_SHOW_IMAGE_SIZES, Form.DEFAULT_SHOW_IMAGE_SIZES));
			
			// Single page form (all fields will be added to a single page):
			if(attributes.getBoolean(Form.DEFAULT_SINGLE_PAGE, ATTRIBUTE_FORM_SINGLE_PAGE))
				newPage(null);
			// Start field:
			formStartFieldId = attributes.getString(ATTRIBUTE_FORM_START_FIELD, null, true, false);
			// skipOnBack:
			currentForm.setSkipOnBack(attributes.getBoolean(ATTRIBUTE_SKIP_ON_BACK, Form.DEFAULT_SKIP_ON_BACK));
			
			//Activate this subtree parser:
			activate(); //!!!
		}
		
		// Within a form...
		else if(currentForm != null)
		{
			// Children of <Form> (controls, fields & triggers)...
			// <Back>
			if(qName.equals(TAG_BACK))
			{
				parseControl(Control.Type.Back, attributes);
			}
			// <Cancel>
			else if(qName.equals(TAG_CANCEL))
			{
				parseControl(Control.Type.Cancel, attributes);
			}
			// <Forward>
			else if(qName.equals(TAG_FORWARD))
			{
				parseControl(Control.Type.Forward, attributes);
			}
			// <Choice>
			else if(qName.equals(TAG_CHOICE))
			{
				newChoice(attributes);
			}
			// <Location>
			else if(qName.equals(TAG_LOCATION))
			{
				newLocation(new LocationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_LOCATION, false)), attributes);
			}
			// <Photo>
			else if(qName.equals(TAG_PHOTO))
			{
				PhotoField photoField = new PhotoField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_PHOTO, false));
				newMediaField(photoField, attributes);
				photoField.setUseNativeApp(attributes.getBoolean("useNativeApp", PhotoField.DEFAULT_USE_NATIVE_APP));
				// Camera options (only used when useNativeApp=false):
				photoField.setUseFrontFacingCamera(attributes.getBoolean("useFrontCamera", PhotoField.DEFAULT_USE_FRONT_FACING_CAMERA));
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
				photoField.setCaptureButtonImageRelativePath(attributes.getString("captureImg", null, false, false));
				photoField.setApproveButtonImageRelativePath(attributes.getString("approveImg", null, false, false));
				photoField.setDiscardButtonImageRelativePath(attributes.getString("discardImg", null, false, false));
			}
			// <Audio>
			else if(qName.equals(TAG_AUDIO))
			{
				AudioField audioField = new AudioField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_AUDIO, false));
				newMediaField(audioField, attributes);
				audioField.setStartRecImageRelativePath(attributes.getString("startRecImg", null, false, false));
				audioField.setStopRecImageRelativePath(attributes.getString("stopRecImg", null, false, false));
			}
			// <Orientation>
			else if(qName.equals(TAG_ORIENTATION))
			{
				OrientationField orField = new OrientationField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_ORIENTATION, false));
				newField(orField, attributes);
				orField.setStoreAzimuth(attributes.getBoolean("storeAzimuth", OrientationField.DEFAULT_STORE_AZIMUTH));
				orField.setStoreAzimuth(attributes.getBoolean("storePitch", OrientationField.DEFAULT_STORE_PITCH));
				orField.setStoreAzimuth(attributes.getBoolean("storeRoll", OrientationField.DEFAULT_STORE_ROLL));
			}
			// <BelongsTo>
			else if(qName.equals(TAG_BELONGS_TO))
			{
				newRelationship(new BelongsToField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID)), attributes);
			}
			// <LinksTo>
			else if(qName.equals(TAG_LINKS_TO))
			{
				newRelationship(new LinksToField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID)), attributes);
			}
			// <Button>
			else if(qName.equals(TAG_BUTTON))
			{
				ButtonField btn = new ButtonField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_BUTTON, true));
				newField(btn, attributes);
				try
				{
					btn.setColumnType(attributes.getString(ATTRIBUTE_BUTTON_COLUMN, ButtonField.DEFAULT_COLUMN_TYPE.name(), true, false));
				}
				catch(IllegalArgumentException iae)
				{
					throw new Exception("Invalid '" + ATTRIBUTE_BUTTON_COLUMN + "' attribute value on <" + TAG_BUTTON + ">.", iae);
				}
				if(btn.getColumnType() == ButtonColumnType.DATETIME && !btn.isOptional())
					addWarning("Button \"" + btn.id + "\" has a DateTime column but is not optional, this means the button will *have* to be pressed.");
			}
			// <Label>
			else if(qName.equals(TAG_LABEL))
			{
				LabelField lbl = new LabelField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_LABEL, true));
				newField(lbl, attributes);
				lbl.setTextSizeScale(attributes.getFloat(ATTRIBUTE_LABEL_SCALE, LabelField.DEFAULT_TEXT_SIZE_SCALE));
				lbl.setCentered(attributes.getBoolean(ATTRIBUTE_LABEL_CENTERED, LabelField.DEFAULT_TEXT_CENTERED));
			}
			// <Text>
			else if(qName.equals(TAG_TEXTFIELD))
			{
				TextBoxField txtField = new TextBoxField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_TEXTFIELD, false));
				newField(txtField, attributes); // first set general things like optionality (needed for getDefaultMinLength() below).
				
				// Deal with minimum & maximum length:
				if(!txtField.isOptional() && !attributes.contains(ATTRIBUTE_TEXT_MINLENGTH))
					addWarning("Text field \"" + txtField.id + "\" is non-optional but no minimal length is defined, therefore the minimum will be set to " + TextBoxField.DEFAULT_MIN_LENGTH_NON_OPTIONAL + " character(s). It is recommended to use the '" + ATTRIBUTE_TEXT_MINLENGTH + "' attribute to set an appropriate minimum length explicitly.");				
				txtField.setMinMaxLength(	attributes.getInteger(ATTRIBUTE_TEXT_MINLENGTH, TextBoxField.GetDefaultMinLength(txtField.isOptional())),
											attributes.getInteger(ATTRIBUTE_TEXT_MAXLENGTH, TextBoxField.DEFAULT_MAX_LENGTH));
				// Multi-line:
				txtField.setMultiline(attributes.getBoolean(ATTRIBUTE_TEXT_MULTILINE, TextBoxField.DEFAULT_MULTILINE));
				
				// Initial value (must happen after min/maxLength are set):
				txtField.setInitialValue(attributes.getString(TextBoxField.GetDefaultInitialValue(txtField.isOptional()), false, true, ATTRIBUTE_FIELD_DEFAULTVALUE, ATTRIBUTE_FIELD_INITVALUE));
				
				// Content types:
				txtField.setContent(attributes.getString(ATTRIBUTE_TEXT_CONTENT, TextBoxField.DEFAULT_CONTENT.name(), true, false));
				
				// Regular expression pattern (to check input against):
				txtField.setRegexPattern(attributes.getString(ATTRIBUTE_TEXT_REGEX, null, false, false));
				
				// Auto capitalisation:
				txtField.setCapitalisation(attributes.getString(ATTRIBUTE_TEXT_CAPITALISATION, TextBoxField.DEFAULT_CAPITALISATION.name(), true, false));
			}
			// <Check>
			else if(qName.equals(TAG_CHECKBOX))
			{
				CheckBoxField chbxField = new CheckBoxField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, TAG_CHECKBOX, false));
				chbxField.setInitialValue(attributes.getBoolean(ATTRIBUTE_FIELD_DEFAULTVALUE, CheckBoxField.DEFAULT_INITIAL_VALUE));
				newField(chbxField, attributes);
			}
			// <List> or <MultiList> (these are in fact just synonyms, but we added both to avoid confusing novice form designers with terminoly that refers to a multi-level list when they only need a flat list)  
			else if(qName.equals(TAG_LIST) || qName.equals(TAG_MULTILIST))
			{
				MultiListField ml = new MultiListField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), readCaption(attributes, qName.equals(TAG_LIST) ? TAG_LIST : TAG_MULTILIST, true, true));
				ml.setPreSelect(attributes.getBoolean(ATTRIBUTE_LIST_PRESELECT, MultiListField.DEFAULT_PRESELECT));
				newField(ml, attributes);
				currentListItem = ml.getItemsRoot();
			}
			// <Page> (Field composite)
			else if(qName.equals(TAG_PAGE))
			{
				newPage(attributes);
			}
			// <Trigger>
			else if(qName.equals(TAG_TRIGGER))
			{
				newTrigger(new Trigger(), attributes);
			}
			// Add future field types here
			//	...
			
			// Tags appearing within Field tags
			else if(!openFields.isEmpty())
			{
				Field currentField = openFields.peek();
				
				// <Argument>
				if(qName.equals(TAG_ARGUMENT))
				{
					parseArgument(currentField, attributes);
				}
				// <Item> (contained within <List> or <MultiList>, and maybe other things later)
				else if(qName.equals(TAG_LISTITEM))
				{
					if(currentListItem != null)
					{
						currentListItem = new MultiListItem(currentListItem, attributes.getRequiredString(TAG_LISTITEM, ATTRIBUTE_FIELD_VALUE, false, true));
						if(attributes.getBoolean(ATTRIBUTE_LISTITEM_DEFAULT, false))
						{
							if(currentListItem.getParent().getDefaultChild() == null)
								currentListItem.getParent().setDefaultChild(currentListItem);
							else
								addWarning("More than 1 item marked as default within one of the (sub)lists of MultiListField " + currentListItem.getField().id + ", using 1st item marked as default as the default for the list.");
						}
					}
					else
						addWarning("Ignored <" + TAG_LISTITEM + "> element occuring outside <" + TAG_LIST + "> or  <" + TAG_MULTILIST + ">.");
				}
				// <Constraint> (contained within <BelongsTo> or <LinksTo>, and maybe other things later)
				else if(qName.equals(TAG_CONSTRAINT))
				{
					if(currentField instanceof Relationship)
					{
						Relationship currentRelationship = (Relationship) currentField;
						String columnName = attributes.getRequiredString(getRelationshipTag(currentRelationship), ATTRIBUTE_CONSTRAINT_COLUMN, true, false);
						
						// Comparison attribute name:
						String comparisonAttrib = null;
						for(String compStr : RuleConstraint.COMPARISON_STRINGS)
							if(attributes.contains(compStr))
							{
								comparisonAttrib = compStr;
								break;
							}
						if(comparisonAttrib == null)
							addWarning("<" + TAG_CONSTRAINT + "> does not contain an comparison attribute (i.e. 1 of: " + StringUtils.join(RuleConstraint.COMPARISON_STRINGS, ", ") + ").");
						else
							owner.addRelationshipConstraint(currentRelationship,
															columnName,
															comparisonAttrib,
															attributes.getRequiredString(getRelationshipTag(currentRelationship), comparisonAttrib, true, true));
					}
					// <Constraint> in something else than <BelongsTo> or <LinksTo>
					else
						addWarning("Ignored <" + TAG_CONSTRAINT + "> element occuring outside <" + TAG_BELONGS_TO + "> or  <" + TAG_LINKS_TO + ">.");
				}
				// <?> within field
				else
				{
					addWarning("Ignored unrecognised or invalidly placed element <" + qName + "> occuring within field with id \"" + currentField.id + "\".");
				}
			}
			
			// Tags appearing within <Trigger>
			else if(openTrigger != null)
			{
				// <Argument>
				if(qName.equals(TAG_ARGUMENT))
				{
					parseArgument(openTrigger, attributes);
				}
				// <?> within trigger
				else
				{
					addWarning("Ignored unrecognised or invalidly placed element <" + qName + "> occuring within <" + TAG_TRIGGER + ">.");
				}
			}
			
			// <?> within <Form>
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
	
	private void parseControl(Control.Type type, XMLAttributes attributes) throws Exception
	{
		// Check if we have not already parsed a tag for this type of control:
		if(parsedControls[type.ordinal()])
			// Yes we have...
			addWarning("More than one occurrence of <" + type.name() + "> found, ignoring all but first");
		else // No we haven't...
		{
			// Remember we parsed this control tag:
			parsedControls[type.ordinal()] = true;

			// Get Control instance to configure:
			Control control = currentForm.getControl(type);
			
			// img:
			control.setImageRelativePath(attributes.getString(ATTRIBUTE_FIELD_IMG, null, false, false));
			
			// backgroundColor:
			control.setBackgroundColor(attributes.getString(ATTRIBUTE_FIELD_BACKGROUND_COLOR, currentForm.getControlBackgroundColor(), true, false));
			
			// Description & audio feedback:
			setDescription(	control.description,
							type.name(),
							attributes.getString(null, true, false, ATTRIBUTE_FIELD_DESC_DESCRIPTION),
							Control.GetDefaultDescriptionText(type),
							null);
		}
	}
	
	/**
	 * Parses a <Choice>
	 * 
	 * @param attributes
	 * @throws Exception
	 */
	private void newChoice(XMLAttributes attributes) throws Exception
	{
		// Parent:
		ChoiceField parent = !openFields.isEmpty() && openFields.peek() instanceof ChoiceField ? (ChoiceField) openFields.peek() : null; 
		
		// Caption:
		String caption = null;
		boolean captionFromAlt = false;
		// 	First try singular caption attributes ("caption" & "label") ...
		caption = attributes.getString(caption, false, true, ATTRIBUTE_FIELD_CAPTION_SINGULAR);
		//	... if that failed, try "alt" (for backwards compatibility), but only if this is not a root choice(!) ...
		if(caption == null && parent != null && attributes.contains(ATTRIBUTE_CHOICE_ALT))
		{
			caption = attributes.getString(ATTRIBUTE_CHOICE_ALT, caption, false, true);
			captionFromAlt = true; // !!!
		} // Note: if neither "caption" nor "alt" appeared the caption variable is still null here
		
		// Create ChoiceField:
		ChoiceField choice = new ChoiceField(currentForm, attributes.getValue(ATTRIBUTE_FIELD_ID), attributes.getValue(ATTRIBUTE_FIELD_VALUE), parent, caption);
		newField(choice, attributes);
		
		// Parse noColumn (for root only):
		if(choice.isRoot())
			choice.setNoColumn(attributes.getBoolean(ATTRIBUTE_FIELD_NO_COLUMN, Field.DEFAULT_NO_COLUMN));
		
		// Parse img path:
		choice.setImageRelativePath(attributes.getString(ATTRIBUTE_FIELD_IMG, null, false, false));
		
		// Caption height:
		//	Parse "captionHeight" attribute (if it exists):
		Float parsedCH = attributes.getFloat(ATTRIBUTE_CHOICE_CAPTION_HEIGHT, null);
		// 	Check if parsed value is not out of bounds:
		if(parsedCH != null && (Float.isNaN(parsedCH) || parsedCH < 0.0f || parsedCH > 1.0f))
		{
			addWarning("Value of attribute " + ATTRIBUTE_CHOICE_CAPTION_HEIGHT + " on <" + TAG_CHOICE  + "> must be in range [0.0, 1.0] (read: " + parsedCH + ").");
			parsedCH = null; // "forget" parsed value
		}
		//	Set Choice caption height:
		choice.setCaptionHeight(parsedCH != null ?
									parsedCH :											// use parsed value
									!choice.isRoot() && choiceParentHadCaptionHeightAttribute ? 
										parent.getCaptionHeight() :						// inherit explicitly specified caption height of parent
										!captionFromAlt ?
											ChoiceField.DEFAULT_CAPTION_HEIGHT : 		// use default height for non-"alt" captions
											ChoiceField.DEFAULT_CAPTION_ALT_HEIGHT);	// use default height for "alt" captions
		//	Set flag to dis/allow inheritance of caption height by children of this choice:
		choiceParentHadCaptionHeightAttribute = parsedCH != null; // we only allow inheritance of explicitly specified, valid captionHeight values
		
		// Text size coordination:
		choice.setMatchTextSize(attributes.getBoolean(ATTRIBUTE_CHOICE_MATCH_TEXT_SIZE,
								choice.isRoot() ?
									ChoiceField.DEFAULT_MATCH_TEXT_SIZE :	// root: use overall default as default
									parent.isMatchTextSize()));				// child: use parent's value as default
		
		// Description & audio feedback:
		//	Question - desc/description/questionDesc/questionDescription is parsed in newField()
		//	Answer   - answerDesc/answerDescription is parsed here, but only for non-root choices:
		if(!choice.isRoot())
			setDescription(	choice.getAnswerDescription(),
							choice.id,
							attributes.getString(null, true, false, ATTRIBUTE_CHOICE_ANSWER_DESC_DESCRIPTION),
							choice.getCaption(),
							"A");
		
		// Other attributes:
		choice.setCols(attributes.getInteger(ATTRIBUTE_CHOICE_COLS, ChoiceField.DEFAULT_NUM_COLS));
		choice.setRows(attributes.getInteger(ATTRIBUTE_CHOICE_ROWS, ChoiceField.DEFAULT_NUM_ROWS));
		choice.setCrossed(attributes.getBoolean(ATTRIBUTE_CHOICE_CROSSED, ChoiceField.DEFAULT_CROSSED));
		choice.setCrossColor(attributes.getString(ATTRIBUTE_CHOICE_CROSS_COLOR, choice.isRoot() ? ChoiceField.DEFAULT_CROSS_COLOR : parent.getCrossColor(), true, false));
	}
	
	/**
	 * @param attributes	may be null for implicit pages (i.e. the one for a singlePage form)
	 * @throws Exception
	 */
	private void newPage(XMLAttributes attributes) throws Exception
	{
		if(!openFields.isEmpty())
			throw new SAXException("<Page> elements must be apprear directly within <Form> and cannot be nested.");
		Page newPage = new Page(currentForm,
								attributes == null ?
									currentForm.id + "_page" :
									attributes.getString(currentForm.id + "_page_" + currentForm.getFields().size(), true, false, ATTRIBUTE_FIELD_ID));
		newField(newPage, attributes);
	}
	
	private void newLocation(LocationField locField, XMLAttributes attributes) throws Exception
	{
		newField(locField, attributes);
		// Location type:
		String type = attributes.getValue("type");
		if("Any".equalsIgnoreCase(type))
			locField.setType(LocationField.TYPE_ANY);
		else if("GPS".equalsIgnoreCase(type))
			locField.setType(LocationField.TYPE_GPS);
		else if("Network".equalsIgnoreCase(type))
			locField.setType(LocationField.TYPE_NETWORK);
		else if(type != null) // unrecognised location type
			addWarning("Unknown Location type (" + type + ").");
		
		// When to start listening for a location:
		String startWith = attributes.getString(ATTRIBUTE_LOCATION_START_WITH, null, true, false);
		if("field".equalsIgnoreCase(startWith))
			locField.setStartWith(LocationField.StartWith.FIELD);
		else if("page".equalsIgnoreCase(startWith))
		{
			if(getCurrentPage() != null)
				locField.setStartWith(LocationField.StartWith.PAGE);
			else
			{
				// told to start on page, but there is no page! Start with field instead (assume the user was trying to avoid "form")
				addWarning("Location field specified to start with page, but no containing page was found. Location detection will start with the field instead.");
				locField.setStartWith(LocationField.StartWith.FIELD);
			}
		}
		else if("form".equalsIgnoreCase(startWith) || attributes.getBoolean(ATTRIBUTE_LOCATION_START_WITH_FORM, false))
			locField.setStartWith(LocationField.StartWith.FORM);
		
		else if (startWith != null)
			// unknown setting, default will be used 
			addWarning("Unknown location field start preference (" + startWith +").");
		
		locField.setWaitAtField(attributes.getBoolean("waitAtField", LocationField.DEFAULT_WAIT_AT_FIELD));
		locField.setTimeoutS(attributes.getInteger("timeout", LocationField.DEFAULT_TIMEOUT_S));
		locField.setMaxAgeS(attributes.getInteger("maxAge", LocationField.DEFAULT_MAX_AGE_S));
		locField.setMaxAccuracyRadius(attributes.getFloat("maxAccuracyRadius", LocationField.DEFAULT_MAX_ACCURACY_RADIUS));
		locField.setUseBestNonQualifyingLocationAfterTimeout(attributes.getBoolean("useBestKnownLocationOnTimeout", LocationField.DEFAULT_USE_BEST_NON_QUALIFYING_LOCATION_AFTER_TIMEOUT));
		// Storage settings:
		locField.setDoublePrecision(attributes.getBoolean("doublePrecision", LocationField.DEFAULT_DOUBLE_PRECISION));
		locField.setStoreAltitude(attributes.getBoolean("storeAltitude", LocationField.DEFAULT_STORE_ALTITUDE));
		locField.setStoreBearing(attributes.getBoolean("storeBearing", LocationField.DEFAULT_STORE_BEARING));
		locField.setStoreSpeed(attributes.getBoolean("storeSpeed", LocationField.DEFAULT_STORE_SPEED));
		locField.setStoreAccuracy(attributes.getBoolean("storeAccuracy", LocationField.DEFAULT_STORE_ACCURACY));
		locField.setStoreProvider(attributes.getBoolean("storeProvider", LocationField.DEFAULT_STORE_PROVIDER));
	}
	
	private void newRelationship(Relationship relationship, XMLAttributes attributes) throws Exception
	{
		newField(relationship, attributes);
		// Remember form name (to resolved later):
		owner.addRelationship(relationship, attributes.getRequiredString(getRelationshipTag(relationship), ATTRIBUTE_RELATIONSHIP_FORM, true, false));
		
		// Other attributes:
		relationship.setHoldForeignRecord(attributes.getBoolean(Relationship.DEFAULT_HOLD_FOREIGN_RECORD, ATTRIBUTE_RELATIONSHIP_REMEMBER, ATTRIBUTE_RELATIONSHIP_HOLD));
		// TODO ? updateStartTimeUponLeave, saveBeforeFormChange, discardBeforeLeave (only for linksTo) ?
	}
	
	private void newMediaField(MediaField ma, XMLAttributes attributes) throws Exception
	{
		newField(ma, attributes);
		ma.setMax(attributes.getInteger(ATTRIBUTE_MEDIA_MAX , MediaField.DEFAULT_MAX));
		if(attributes.getValue(ATTRIBUTE_DISABLE_FIELD) != null)
			mediaAttachToDisableId.put(ma, attributes.getValue(ATTRIBUTE_DISABLE_FIELD).trim().toUpperCase()); // upper cased, for case insensitivity
	}
	
	/**
	 * Adds field to current currentForm or currentPage, sets optionalness, remembers id & jump & reads various Field attributes
	 * 
	 * @param field		the Field object
	 * @param attributes	may be null for implicit fields (fields that are inserted by the parser but do not explicitly appear in the XML, e.g. the Page for a singlePage form) 
	 * @throws Exception
	 */
	private void newField(Field field, XMLAttributes attributes) throws Exception
	{
		try
		{	
			// Warn about IDs starting with '_':
			if(field.id.startsWith("_"))
			{
				// For really stupid cases ;-):
				for(EndField ef : EndField.GetEndFields(currentForm))
					if(ef.id.equals(field.id))
						throw new SAXException(field.id + " is a reserved ID, don't use it for user-defined fields.");
				addWarning("Please avoid field IDs starting with '_' (" + field.id + ")."); 
			}
			// Note: warnings about chars that are illegal in column-name/XML-tags will be generated in Field#getColumn()
			
			// Remember the field ID (upper cased, for case insensitivity), in order to check for duplicates (now) & resolve jumps (later):
			if(idToField.put(field.id.toUpperCase(), field) != null)
				throw new SAXException("Duplicate field ID '" + field.id + "' in form '" + currentForm.id + "'! (Note: field and form IDs are case insensitive)");
			
			// Get current page if there is one:
			Page currentPage = getCurrentPage();
			
			// If the field is a root field (note: even elements on a page are root fields)...
			if(field.isRoot())
			{
				// Add it to the form or page:
				if(currentPage == null)
					// field is top-level (directly contained within the form, and not in a page first):
					currentForm.addField(field);
				else
					// the field is contained by a page:
					currentPage.addField(field);
				
				if(attributes != null)
				{
					// Set optionalness:
					String optText = attributes.getValue(ATTRIBUTE_FIELD_OPTIONAL);
					boolean opt = currentPage == null ? Field.DEFAULT_OPTIONAL : currentPage.isOptional(); // use default optionalness or that of the containing page
					if(optText != null && !optText.trim().isEmpty())
					{	
						optText = optText.trim();
						if("always".equalsIgnoreCase(optText) || Boolean.TRUE.toString().equalsIgnoreCase(optText))
							opt = true;
						else if("notIfReached".equalsIgnoreCase(optText)) // deprecated, but still parsed on all format versions (for backwards compatibility)
							opt = false;
						else if("never".equalsIgnoreCase(optText) || Boolean.FALSE.toString().equalsIgnoreCase(optText))
							opt = false;
					}
					field.setOptional(opt);
					
					// Show on create/edit:
					field.setShowOnCreate(attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_ON_CREATE, Field.DEFAULT_SHOW_ON_CREATE));
					field.setShowOnEdit(attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_ON_EDIT, Field.DEFAULT_SHOW_ON_EDIT));
					
					// Set editable (inherit from page if on page):
					field.setEditable(attributes.getBoolean(ATTRIBUTE_FIELD_EDITABLE, currentPage == null ? Field.DEFAULT_EDITABLE : currentPage.isEditable()));
				}
			}
		
			// Read various optional Field attributes: 
			if(attributes != null)
			{	
				// Remember jumps (always "intra-Form", and not leaving a page unless this type of field is allowed to do that):
				if(attributes.getValue(ATTRIBUTE_FIELD_JUMP) != null)
				{
					if(currentPage == null || field.canJumpFromPage())
						jumpSourceToJumpTargetId.put(field, attributes.getValue(ATTRIBUTE_FIELD_JUMP).trim().toUpperCase()); // trimmed (because id's on fields are too) & upper cased (for case insensitivity)
					else if(currentPage != null)
						addWarning("Field \"" + field.id + "\" tries to jump away from the page, but is not allowed.");
				}
				
				// Skip on back:
				field.setSkipOnBack(attributes.getBoolean(ATTRIBUTE_SKIP_ON_BACK, Field.DEFAULT_SKIP_ON_BACK));
				
				// Background colour:
				field.setBackgroundColor(attributes.getString(ATTRIBUTE_FIELD_BACKGROUND_COLOR, field.getBackgroundColor(), true, false));
				
				// Which buttons are allowed to show...
				// 	Mode-specific:
				field.setShowControlOnMode(Control.Type.Back, Mode.CREATE, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_BACK_ON_CREATE, Field.DEFAULT_SHOW_BACK));
				field.setShowControlOnMode(Control.Type.Back, Mode.EDIT, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_BACK_ON_EDIT, Field.DEFAULT_SHOW_BACK));
				field.setShowControlOnMode(Control.Type.Cancel, Mode.CREATE, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_CANCEL_ON_CREATE, Field.DEFAULT_SHOW_CANCEL));
				field.setShowControlOnMode(Control.Type.Cancel, Mode.EDIT, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_CANCEL_ON_EDIT, Field.DEFAULT_SHOW_CANCEL));
				field.setShowControlOnMode(Control.Type.Forward, Mode.CREATE, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_FORWARD_ON_CREATE, Field.DEFAULT_SHOW_FORWARD));
				field.setShowControlOnMode(Control.Type.Forward, Mode.EDIT, attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_FORWARD_ON_EDIT, Field.DEFAULT_SHOW_FORWARD));		
				//	Across all modes (overrules mode-specific settings) + with backwards compatibility for v1.0 forms which may have showBack/showCancel/showForward at the form level:
				if(attributes.contains(ATTRIBUTE_FIELD_SHOW_BACK) || v1xFormShowBack != null)
					field.setShowBack((v1xFormShowBack != null ? v1xFormShowBack : true) && attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_BACK, Field.DEFAULT_SHOW_BACK));
				if(attributes.contains(ATTRIBUTE_FIELD_SHOW_CANCEL) || v1xFormShowCancel != null)
					field.setShowCancel((v1xFormShowCancel != null ? v1xFormShowCancel : true) && attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_CANCEL, Field.DEFAULT_SHOW_CANCEL));
				if(attributes.contains(ATTRIBUTE_FIELD_SHOW_FORWARD) || v1xFormShowForward != null)
					field.setShowForward((v1xFormShowForward != null ? v1xFormShowForward : true) && attributes.getBoolean(ATTRIBUTE_FIELD_SHOW_FORWARD, Field.DEFAULT_SHOW_FORWARD));

				// Description (= "question description" for ChoiceFields):
				setDescription(	field.description,
								field.id,
								attributes.getString(null, true, false,  field instanceof ChoiceField ? ATTRIBUTE_CHOICE_QUESTION_DESC_DESCRIPTION : ATTRIBUTE_FIELD_DESC_DESCRIPTION),
								field instanceof ChoiceField ? null : field.getCaption(),
								field instanceof ChoiceField ? "Q" : null);
			}

			// Remember current field:
			openFields.push(field); //!!!
		}
		catch(Exception e)
		{
			throw new Exception("Error on parsing field '" + field.id + "'", e);
		}
	}
	
	/**
	 * Helper method to set the textual and/or audioRelativePath members of a {@link Description} object.
	 * 
	 * @param identification field id or control type name
	 * @param textOrPath a String which may either be null, OR a path to a sound file packaged with the project (relative to the snd/ folder, although we do not check here whether such a file actually exists), OR a readable/pronounceable piece of text to be used for speech synthesis
	 * @param fallbackText alternative text to be used for speech synthesis
	 * @param fileNamePostfix additional String to be appended to name of generated audio files
	 * @return a String which is null if description was null or the form doesn't have audio feedback enabled, OR the description itself if it was sound file path already, OR the path (relative to the snd/ folder) or a to-be-generated sound file containing speech synthesised from the description 
	 */
	private void setDescription(Description description, String identification, String textOrPath, String fallbackText, String fileNamePostfix)
	{
		// Normalise empty strings to null:
		textOrPath = StringUtils.emptyToNull(textOrPath);
		fallbackText = StringUtils.emptyToNull(fallbackText);
		fileNamePostfix = StringUtils.emptyToNull(fileNamePostfix);

		// Check if textOrPath contains a path to an audio file (which does not necessarily exist though):
		boolean prerecordedAudio = MediaHelpers.isAudioFileName(textOrPath);
		
		// Set textual description:
		if(textOrPath != null && !prerecordedAudio)
			// this means textOrPath is a readable/pronounceable text, and not a path
			description.setText(textOrPath);
		else if(fallbackText != null)
			// use fallbackText:
			description.setText(fallbackText);
		
		// Set audio relative path (for audio feedback):
		if(currentForm.isUsingAudioFeedback())
		{
			if(prerecordedAudio)
				// Playback of prerecorded audio (file included with project):
				description.setAudioRelativePath(textOrPath);
			else
			{	// Audio file will have to be generated:
				if(textOrPath != null || fallbackText != null)
				{
					// Playback of audio generated from text (TTS):
					String languageCode = currentForm.getDefaultLanguage();
					String toPronounce = textOrPath != null ? textOrPath : fallbackText;
					String relativeSoundFilePath =
						// Filename: "[form.id]_[identification]_[UpperCase(Hex(hashCode(toPronounce)))]_[language][_postfix].[EXTENSION]"
						FileHelpers.makeValidFileName(	currentForm.id +
														"_" + identification +
														"_" + Integer.toHexString(toPronounce.hashCode()).toUpperCase() +
														"_" + languageCode +
														(fileNamePostfix != null ? "_" + fileNamePostfix : "") +
														"." + owner.getGeneratedAudioExtension());
					// Add synthesis task to be executed during post-processing:
					owner.addPostProcessingTask(new TTVSynthesisTask(toPronounce, relativeSoundFilePath, languageCode));
					description.setAudioRelativePath(relativeSoundFilePath);
				}
				//else: there is no text to synthesise (nor a path to a pre-recorded audio file), so don't set the audio relative path
			}
		}
	}
	
	private void newTrigger(Trigger trigger, XMLAttributes attributes)
	{
		// Parse the attributes
		String keys = attributes.getString(null, true, false, ATTRIBUTE_TRIGGER_KEY, ATTRIBUTE_TRIGGER_KEYS);
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
		trigger.setFixedTimer(attributes.getInteger(ATTRIBUTE_TRIGGER_FIXED_TIMER, Trigger.NO_TIMEOUT));
		if(attributes.contains(ATTRIBUTE_TRIGGER_JUMP)) // Remember jump (always "intra-Form")
			jumpSourceToJumpTargetId.put(trigger, attributes.getValue(ATTRIBUTE_TRIGGER_JUMP).trim().toUpperCase()); // upper cased, for insensitivity
		
		// Add the trigger to the current Page
		Page currentPage = getCurrentPage();
		if(currentPage != null)
			currentPage.addTrigger(trigger);
		// else add the triggers to the Form
		else
			currentForm.addTrigger(trigger);
		
		// Remember trigger (so arguments can be added):
		openTrigger = trigger;
	}
	
	private void parseArgument(JumpSource source, XMLAttributes tagAttributes) throws Exception
	{
		if(!source.hasNextFieldArguements())
			source.setNextFieldArguments(new FieldParameters());
		source.getNextFieldArguments().put(	tagAttributes.getRequiredString(TAG_ARGUMENT, ATTRIBUTE_ARGUMENT_PARAM, true, false),
											tagAttributes.getRequiredString(TAG_ARGUMENT, ATTRIBUTE_ARGUMENT_VALUE, false, true));
		// TODO Let Field instance validate param & value?
	}

	private Page getCurrentPage()
	{
		// Iterate through the stack from the top down and look for a Page:
		for(int f = openFields.size() - 1; f >= 0; f--)
			if(openFields.get(f) instanceof Page)
				return (Page) openFields.get(f);
		// No current page:
		return null;
	}
	
	protected void closePage(Page page)
	{
		/* The 'optional' attribute of a page is only used to inherit from by contained fields (see newField()),
		 * at runtime it doesn't have meaning in itself because the page does not have a column of its own and
		 * whether or not the page can be skipped or left is to be decided based on the optionalness and acquired
		 * values of the contained fields.
		 * Because of this the optionalness of the page is reset to ALWAYS after all contained fields are parsed.
		 */
		page.setOptional(true);
	}
	
	@Override
	protected void parseEndElement(String uri, String localName, String qName) throws Exception
	{
		// Close field: </Choice>, </Location>, </Photo>, </Audio>, </Orientation>, </BelongsTo>, </LinksTo>, </Button>, </Label>, </Textbox>, </Checkbox>, </List>, </MultiList>, </Page>
		if(	!openFields.isEmpty() && (
			qName.equals(TAG_CHOICE) || qName.equals(TAG_LOCATION) ||
			qName.equals(TAG_PHOTO) || qName.equals(TAG_AUDIO) ||
			qName.equals(TAG_ORIENTATION) || qName.equals(TAG_BELONGS_TO) ||
			qName.equals(TAG_LINKS_TO) || qName.equals(TAG_BUTTON) ||
			qName.equals(TAG_LABEL) || qName.equals(TAG_TEXTFIELD) ||
			qName.equals(TAG_CHECKBOX) || qName.equals(TAG_LIST) ||
			qName.equals(TAG_MULTILIST) || qName.equals(TAG_PAGE)))
		{
			Field currentField = openFields.pop(); // pop the field
			
			// </Choice>
			if(qName.equals(TAG_CHOICE) && currentField instanceof ChoiceField)
			{
				ChoiceField currentChoice = (ChoiceField) currentField;
				if(currentChoice.isRoot() && currentChoice.isLeaf())
					throw new SAXException("Root choices need at least 1 child (but 2 or more children probably makes more sense).");
			}
			// </Page>
			else if(qName.equals(TAG_PAGE) && currentField instanceof Page)
				closePage((Page) currentField);
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
		
		// </Trigger>
		else if(qName.equals(TAG_TRIGGER))
		{
			openTrigger = null;
		}
		
		// </Form>
		else if(qName.equals(TAG_FORM))
		{
			// close page in case of a singePage form:
			Page currentPage = getCurrentPage();
			if(currentPage != null)
			{
				closePage(currentPage);
				openFields.pop();
			}
			
			// Resolve/set currentForm start field:
			Field startField = currentForm.getFields().get(0); // first field is the default start field
			if(formStartFieldId != null) // try with field specified by ID in <Form startField="..."> (may be null)
			{
				Field specifiedStartField = currentForm.getField(formStartFieldId); // uses equalsIgnoreCase()
				if(specifiedStartField == null) // TODO throw exception instead
					addWarning("The specified start field (\"" + formStartFieldId + "\") of currentForm \"" + currentForm.getName() + "\" does not exist, using first field instead.");
				else
					startField = specifiedStartField;
			}
			currentForm.setStartField(startField);		
			
			// Add EndField instances to idToField map (these don't need to be added as actual fields to the form itself)
			for(EndField endF : EndField.GetEndFields(currentForm))
				idToField.put(endF.id.toUpperCase(), endF); // upper cased, for case insensitivity (they should already be upper case, but just in case...)
			
			// Resolve jumps...
			for(Entry<JumpSource, String> jump : jumpSourceToJumpTargetId.entrySet())
			{
				Field target = idToField.get(jump.getValue());
				if(target == null)
					addWarning("Cannot resolve jump ID '" + jump.getValue() +  "' (case insensitive).");
				else if(target.isOnPage())
					addWarning("Cannot jump to field '" + target.id +  "' because it is contained within a page.");
				else
					jump.getKey().setJump(target); // set jump pointer (to a field object)
				// Note: jumping to Choice children is allowed (i.e. choice-graphs, rather than trees, are possible)
				// TODO we may want to check whether/when jumping to Choice children is desirable (e.g. only from within the tree/graph, or from anywhere?), and rule out certain situations by additional checks here (like the isOnPage() one above)
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
			
			// Generate (audio) descriptions for missing Control tags:
			for(Control.Type type : Control.Type.values())
				if(!parsedControls[type.ordinal()])
					setDescription(	currentForm.getControl(type).description,
									type.name(),
									Control.GetDefaultDescriptionText(type),
									null,
									null);

			// Deactivate this subtree parser:
			deactivate(); //will call reset() (+ warnings will be copied to owner)
		}
	}

	private String readCaption(XMLAttributes tagAttributes, String tag, boolean required) throws Exception
	{
		return readCaption(tagAttributes, tag, required, false); // singular by default
	}
	
	private String readCaption(XMLAttributes tagAttributes, String tag, boolean required, boolean plural) throws Exception
	{
		if(required)
			return tagAttributes.getRequiredString(tag, false, true, plural ? ATTRIBUTE_FIELD_CAPTION_PLURAL : ATTRIBUTE_FIELD_CAPTION_SINGULAR);
		else
			return tagAttributes.getString(null, false, true, plural ? ATTRIBUTE_FIELD_CAPTION_PLURAL : ATTRIBUTE_FIELD_CAPTION_SINGULAR);
	}
	
	private String getRelationshipTag(Relationship relationship)
	{
		if(relationship instanceof BelongsToField)
			return TAG_BELONGS_TO;
		if(relationship instanceof LinksToField)
			return TAG_LINKS_TO;
		throw new IllegalArgumentException("Unsupported relationship type");
	}

	@Override
	protected boolean isSingleUse()
	{
		return false;
	}

}
