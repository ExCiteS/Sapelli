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

package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.VideoField;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ButtonUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.CheckBoxUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ChoiceUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LabelUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LocationUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MediaUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MultiListUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.OrientationUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.PageUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.TextBoxUI;

public interface CollectorUI<V, UI extends CollectorUI<V, UI>>
{
	
	public FieldUI<? extends Field, V, UI> getFieldUI(Field field);

	public ChoiceUI<V, UI> createChoiceUI(ChoiceField cf);

	public MediaUI<PhotoField,V, UI> createPhotoUI(PhotoField pf);
	
	public MediaUI<VideoField,V, UI> createVideoUI(VideoField pf);

	public MediaUI<AudioField,V, UI> createAudioUI(AudioField af);

	public LocationUI<V, UI> createLocationUI(LocationField lf);
	
	public OrientationUI<V, UI> createOrientationUI(OrientationField of);
	
	public LabelUI<V, UI> createLabelUI(LabelField lf);
	
	public ButtonUI<V, UI> createButtonUI(ButtonField bf);

	public TextBoxUI<V, UI> createTextFieldUI(TextBoxField tf);
	
	public CheckBoxUI<V, UI> createCheckBoxFieldUI(CheckBoxField cbf);
	
	public MultiListUI<V, UI> createMultiListUI(MultiListField mlf);
	
	public PageUI<V, UI> createPageUI(Page page);
	
	public void setField(Field currentField);
	
	public FieldUI<?, V, UI> getCurrentFieldUI();
	
	public int getSpacingPx();
	
	public int getScreenWidthPx();
	
	public int getScreenHeightPx();
	
	public ControlsUI<V, UI> getControlsUI();
	
	public AudioFeedbackController<V> getAudioFeebackController();
	
	/**
	 * Stop audio feedback playback
	 */
	public void stopAudioFeedback();

	/**
	 * Stop audio feedback playback & release associated resources
	 */
	public void destroyAudioFeedback();
	
}