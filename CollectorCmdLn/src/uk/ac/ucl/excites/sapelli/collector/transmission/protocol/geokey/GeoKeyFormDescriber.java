/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.BelongsToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
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
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import uk.ac.ucl.excites.sapelli.storage.types.OrientationColumn;

/**
 * @author mstevens
 * @see https://github.com/ExCiteS/geokey-sapelli
 */
public class GeoKeyFormDescriber implements FieldVisitor
{

	private final JsonNodeFactory factory = new JsonNodeFactory(false);
	private ArrayNode fieldNodes;
	private ArrayNode locationNodes;
	
	public ObjectNode getFormJSON(Form form)
	{
		// Initialise:
		fieldNodes = factory.arrayNode();
		locationNodes = factory.arrayNode();
		
		// Check:
		if(!form.isProducesRecords())
			return null;
		
		// Visit fields:
		for(Field field : form.getFields())
			field.enter(this, null, false);
		
		// Create & return JSON node:
		ObjectNode formNode = factory.objectNode();
		formNode.put("sapelli_id", form.id);
		formNode.set("fields", fieldNodes);
		formNode.set("locations", locationNodes);
		formNode.put("stores_end_time", form.isStoreEndTime());
		formNode.put("sapelli_model_schema_number", form.getProject().getModel().getSchemaNumber(form.getSchema()));
		return formNode;
	}
	
	private boolean addFieldNode(ObjectNode fieldNode)
	{
		fieldNodes.add(fieldNode);
		return true;
	}
	
	public ObjectNode baseField(Field f, String geokeyType)
	{
		if(f.isNoColumn())
			return null;
		return baseField(	f.getColumn().name,
							f.getColumn().isOptional(),
							f.getColumn() instanceof BooleanColumn,
							f.getCaption(),
							f.description.getText(),
							geokeyType);
	}
	
	public ObjectNode baseField(String columnName, boolean optional, boolean bool, String caption, String description, String geokeyType)
	{
		return factory.objectNode()
		.put("sapelli_id", columnName)
		.put("required", !optional)
		.put("truefalse", bool)
		.put("caption", caption)
		.put("description", description)
		.put("geokey_type", geokeyType);
	}
	
	public ObjectNode booleanField(Field f)
	{
		ObjectNode fieldNode = baseField(f, "LookupField");
		if(fieldNode == null)
			return null;
		ArrayNode itemNodes = factory.arrayNode();
		itemNodes.add(factory.objectNode().put("value", "true"));
		itemNodes.add(factory.objectNode().put("value", "false"));
		fieldNode.set("items", itemNodes);
		
		return fieldNode;
	}
	
	@Override
	public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments, boolean withPage)
	{
		ObjectNode fieldNode = baseField(cf, "LookupField");
		if(fieldNode == null)
			return false;
		// Items (= leaf choices):
		ArrayNode itemNodes = factory.arrayNode();
		for(ChoiceField leaf : cf.getDictionary().getItems())
			itemNodes.add(	factory.objectNode()
							.put("value", leaf.getValue())
							.put("img", leaf.getImageRelativePath()));
		fieldNode.set("items", itemNodes);
		
		return addFieldNode(fieldNode);
	}

	@Override
	public boolean enterMultiListField(MultiListField mlf, FieldParameters arguments, boolean withPage)
	{
		ObjectNode fieldNode = baseField(mlf, "LookupField");
		if(fieldNode == null)
			return false;
		// Items:
		ArrayNode itemNodes = factory.arrayNode();
		for(MultiListItem item : mlf.getDictionary().getItems())
			itemNodes.add(	factory.objectNode()
							.put("value", item.getValue())
							.set("img", null));
		fieldNode.set("items", itemNodes);
		
		return addFieldNode(fieldNode);
	}
	
	@Override
	public boolean enterLocationField(LocationField lf, FieldParameters arguments, boolean withPage)
	{
		locationNodes.add(baseField(lf, null));
		return true;
	}

	@Override
	public boolean enterOrientationField(OrientationField of, FieldParameters arguments, boolean withPage)
	{
		OrientationColumn orCol = (OrientationColumn) of.getColumn();
		addFieldNode(baseField(orCol.getQualifiedSubColumnName(Orientation.COLUMN_AZIMUTH),	orCol.optional || !of.isStoreAzimuth(),	false, of.getCaption(), of.description.getText(), "NumericField"));
		addFieldNode(baseField(orCol.getQualifiedSubColumnName(Orientation.COLUMN_PITCH),	orCol.optional || !of.isStorePitch(),	false, of.getCaption(), of.description.getText(), "NumericField"));
		addFieldNode(baseField(orCol.getQualifiedSubColumnName(Orientation.COLUMN_ROLL),	orCol.optional || !of.isStoreRoll(),	false, of.getCaption(), of.description.getText(), "NumericField"));
		return true;
	}

	@Override
	public boolean enterPage(Page page, FieldParameters arguments)
	{
		for(Field field : page.getFields())
			field.enter(this, null, true);
		return true;
	}


	@Override
	public boolean enterBelongsTo(BelongsToField belongsTo, FieldParameters arguments)
	{
		ForeignKeyColumn fkCol = (ForeignKeyColumn) belongsTo.getColumn();
		addFieldNode(baseField(fkCol.getQualifiedSubColumnName(Form.COLUMN_TIMESTAMP_START), false,	false, belongsTo.getRelatedForm().id + " Start Time", belongsTo.description.getText(), "DateTimeField"));
		addFieldNode(baseField(fkCol.getQualifiedSubColumnName(Form.COLUMN_DEVICE_ID),		 false,	false, belongsTo.getRelatedForm().id + " Device Id",  belongsTo.description.getText(), "NumericField"));
		return true;
	}

	@Override
	public boolean enterTextBoxField(TextBoxField tbf, FieldParameters arguments, boolean withPage)
	{
		String geokeyType = null;
		switch(tbf.getContent())
		{
			case text :
			case password :
			case email :
			case phonenumber :
			default :
				geokeyType = "TextField";
				break;
			case unsignedint :
			case signedint :
			case unsignedlong :
			case signedlong :
			case unsignedfloat :
			case signedfloat :
			case unsigneddouble :
			case signeddouble :
				geokeyType = "NumericField";
				break;
		}
		ObjectNode fieldNode = baseField(tbf, geokeyType);
		if(fieldNode == null)
			return false;
		
		return addFieldNode(fieldNode);
	}
	
	@Override
	public boolean enterCheckboxField(CheckBoxField cbf, FieldParameters arguments, boolean withPage)
	{
		return addFieldNode(booleanField(cbf));
	}
	
	@Override
	public boolean enterButtonField(ButtonField buttonField, FieldParameters arguments, boolean withPage)
	{
		switch(buttonField.getColumnType())
		{
			case BOOLEAN :
				return addFieldNode(booleanField(buttonField));
			case DATETIME :
				return addFieldNode(baseField(buttonField, "DateTimeField"));
			case NONE :
			default :
				return false;
		}
	}

	@Override
	public boolean enterMediaField(MediaField mf, FieldParameters arguments, boolean withPage)
	{
		return false;
	}
	
	@Override
	public boolean enterLinksTo(LinksToField linksTo, FieldParameters arguments)
	{
		return false;
	}

	@Override
	public boolean enterLabelField(LabelField lblf, FieldParameters arguments, boolean withPage)
	{
		return false;
	}

	@Override
	public boolean enterEndField(EndField ef, FieldParameters arguments)
	{
		return false;
	}
	
}