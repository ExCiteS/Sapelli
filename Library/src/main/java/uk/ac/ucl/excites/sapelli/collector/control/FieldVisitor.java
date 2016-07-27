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

package uk.ac.ucl.excites.sapelli.collector.control;

import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
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
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;

/**
 * 
 * @author mstevens
 */
public interface FieldVisitor
{

	/**
	 * @param cf  the ChoiceField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments, boolean withPage);

	/**
	 * @param mf  the MediaField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMediaField(MediaField mf, FieldParameters arguments, boolean withPage);

	/**
	 * @param lf  the LocationField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterLocationField(LocationField lf, FieldParameters arguments, boolean withPage);

	/**
	 * @param of  the OrientationField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterOrientationField(OrientationField of, FieldParameters arguments, boolean withPage);

	/**
	 * @param page	the Page
	 * @param arguments
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterPage(Page page, FieldParameters arguments);

	/**
	 * @param linksTo
	 * @param arguments
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterLinksTo(LinksToField linksTo, FieldParameters arguments);

	/**
	 * @param belongsTo
	 * @param arguments
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterBelongsTo(BelongsToField belongsTo, FieldParameters arguments);

	/**
	 * @param tbf  the TextBoxField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterTextBoxField(TextBoxField tbf, FieldParameters arguments, boolean withPage);
	
	/**
	 * @param cbf  the CheckBoxField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterCheckboxField(CheckBoxField cbf, FieldParameters arguments, boolean withPage);
	
	/**
	 * @param lblf  the LabelField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterLabelField(LabelField lblf, FieldParameters arguments, boolean withPage);
	
	/**
	 * @param ef  the EndField
	 * @param arguments
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef, FieldParameters arguments);

	/**
	 * @param buttonField
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterButtonField(ButtonField buttonField, FieldParameters arguments, boolean withPage);
	
	/**
	 * @param mlf
	 * @param arguments
	 * @param withPage whether or not the field is entered together with a page that contains it, or entered on its own
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMultiListField(MultiListField mlf, FieldParameters arguments, boolean withPage);

}