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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;

/**
 * @author mstevens
 *
 */
public abstract class ChoiceUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<ChoiceField, V, UI>
{

	static public final String MISSING_CAPTION_TEXT = "?";
	
	/**
	 * @param field
	 * @param controller
	 * @param collectorUI
	 */
	public ChoiceUI(ChoiceField choice, CollectorController<UI> controller, UI collectorUI)
	{
		super(choice, controller, collectorUI);
		if(choice.isLeaf()) // just in case...
			throw new IllegalArgumentException("Cannot display leaf choice.");
	}
	
	/**
	 * Note 1:	chosenChild is not the (current) field! The field (also a ChoiceField) is its parent.
	 * 
	 * Note 2:	For leaves we cannot just call goForward() here because then we would first need to make
	 * 			the chosenChild the currentField, in which case it would end up in the fieldHistory which
	 * 			does not make sense because a leaf choice cannot be displayed on its own.
	 * 
	 * Note 3:	For the same reason leaves are never actually "visited" by the controller, instead we
	 * 			*go* straight *to* what follows (the next/jump of the leaf).
	 * 
	 * Note 4:	If the chosenChild is not a leaf, or it is a "valueless" leaf, the goTo is unconditional
	 * 			because otherwise validation would keep us from advancing. If it is a "valued" leaf
	 * 			validation will happen. This means valueless leaves offer way out of choice trees, even
	 * 			non-optional ones. Form designers should use this with care (e.g. only using valueless
	 * 			leaves as "back jumps", and assuring the field will be revisited to acquire a value).
	 *
	 * @param chosenChild
	 */
	protected void choiceMade(ChoiceField chosenChild)
	{
		if(!controller.isFieldEnabled(chosenChild))
			throw new IllegalArgumentException("This choice is disabled:" + chosenChild.id); // should never happen

		controller.addLogLine("CLICKED", chosenChild.toString(true));

		FieldWithArguments next;
		if(chosenChild.isLeaf())
		{	// Store value if the field has a column, the chosenChild is a leaf and it is known in the field dictionary (meaning it carries a value):
			if(!field.isNoColumn() && chosenChild.isLeaf() && field.getDictionary().contains(chosenChild))
				field.getColumn().storeValue(controller.getCurrentRecord(), field.getDictionary().lookupIndex(chosenChild));
			// Go to next/jump of chosenChild (not to the chosen child itself because it is a leaf):
			next = field.form.getNextFieldAndArguments(chosenChild, true);
		}
		else
			// Go to chosen child:
			next = new FieldWithArguments(chosenChild, field.getNextFieldArguments()); // No arguments (i.e. FieldParameters) are passed from parent to child
		// Go...
		controller.goTo(next, chosenChild.isLeaf() && field.getDictionary().contains(chosenChild) ? LeaveRule.CONDITIONAL : LeaveRule.UNCONDITIONAL_NO_STORAGE);
	}
	
	/**
	 * Returns the String to display as the caption under an image or instead of an image when no image was specified
	 * 
	 * @param choice
	 * @param allowCaption whether the ChoiceField.caption can be used (because it is isn't already displayed above the caption item)
	 * @return text to use a caption (not necessarily taken from choice#caption)
	 */
	protected String getCaptionText(ChoiceField choice, boolean allowCaption)
	{
		if(choice.hasCaption() && allowCaption) // only use caption if it is not already displayed underneath the image, or above it in a page label
			return choice.getCaption();
		if(choice.getValue() != null)
			return choice.getValue();
		return MISSING_CAPTION_TEXT;
	}
	
	/**
	 * Returns the String to display *instead* of an image which the form designer
	 * wanted to show (meaning that choice#imageRelativePath is not null) but which
	 * cannot be displayed (due to missing/inaccessible file).
	 * 
	 * @param choice
	 * @param standAlone whether the text will be displayed on its own, or not (i.e. under an image or under a page caption-label)
	 * @return text to display instead of a missing image
	 */
	protected String getAltText(ChoiceField choice, boolean standAlone)
	{
		if(choice.hasCaption() && standAlone) // only use caption if it is not already displayed underneath the image, or above it in a page label
			return choice.getCaption();
		if(choice.getValue() != null)
			return choice.getValue();
		return choice.getImageRelativePath();
	}
	
	/**
	 * @return a textual answer description (only for non-root choices)
	 */
	public String getAnswerDescriptionText(ChoiceField choice)
	{
		// Roots are never answers:
		if(choice.isRoot())
			return null;
		// Try textual answer description: based on "answerDesc[ription]" <Choice> attribution, with fall-back to "caption" or "alt" attributes
		if(choice.getAnswerDescription().getText() != null)
			return choice.getAnswerDescription().getText();
		// Additional fall-back:
		if(choice.getValue() != null)
			return choice.getValue();
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isFieldUsingAudioFeedback(boolean)
	 */
	@Override
	protected boolean isFieldUsingAudioFeedback(boolean withPage)
	{
		return !withPage; // do not use if on page (yet)
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#getAudioFeedbackJobs(uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback, boolean)
	 */
	@Override
	protected abstract List<AudioFeedbackController<V>.PlaybackJob> getAudioFeedbackJobs(AudioFeedback audioFeedbackMode, boolean withPage);

}
