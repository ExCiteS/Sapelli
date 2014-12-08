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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.State;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Abstract class to represent the UI of a Field
 * 
 * @author mstevens
 *
 * @param <F>
 * @param <V>
 * @param <UI>
 */
public abstract class FieldUI<F extends Field, V, UI extends CollectorUI<V, UI>>
{
	
	protected final F field;
	protected final Controller<UI> controller;
	protected final UI collectorUI;
	private boolean shown = false;
	
	private Record lastKnownRecord = null;
	
	public FieldUI(F field, Controller<UI> controller, UI collectorUI)
	{
		this.field = field;
		this.controller = controller;
		this.collectorUI = collectorUI;
	}
	
	public final F getField()
	{
		return field;
	}
	
	/**
	 * Returns a platform-specific UI element (e.g. an Android View instance),
	 * the object may be recycled but should be updated w.r.t. the provided record.
	 * 
	 * @param onPage
	 * @param record
	 * @return
	 */
	public V showField(boolean onPage, Record record)
	{
		// Check if record is new:
		boolean newRecord = (lastKnownRecord != record);
		
		// Remember record:
		lastKnownRecord = record;
		
		// Mark the fieldUI as currently shown:
		this.shown = true;
		
		return getPlatformView(onPage, controller.isFieldEnabled(field), record, newRecord);
	}
	
	/**
	 * Returns a platform-specific UI element (e.g. an Android View instance),
	 * the object may be recycled but should be updated w.r.t. the provided record.
	 * 
	 * @param onPage
	 * @parem enabled
	 * @param record
	 * @param newRecord whether or not this is a new record
	 * @return
	 */
	protected abstract V getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord);
	
	/**
	 * Hides/cancels the fieldUI
	 */
	public void hideField()
	{
		// mark fieldUI as *not* currently shown:
		this.shown = false;
		// Run cancel behaviour:
		cancel();
	}
	
	/**
	 * To be overridden by FieldUIs that need to execute cancelling behaviour before disappearing off the screen
	 */
	protected void cancel()
	{
		// does nothing by default!
	}
	
	/**
	 * Request to leave the field.
	 * 
	 * @param record
	 * @param rule determines whether the leaving is (un)conditional and whether validation/storage is allowed to take place
	 * @return whether or not leaving the field is allowed
	 */
	public boolean leaveField(Record record, Controller.LeaveRule rule)
	{
		if(	// when the request is unconditional AND no storage is allowed we don't call leave() so no validation/storage happens:
				(rule == LeaveRule.UNCONDITIONAL_NO_STORAGE)
				// otherwise we allow validation/storage to happen but it only affects the leave permission if the request is conditional:
				||	(rule == LeaveRule.UNCONDITIONAL_WITH_STORAGE | leave(record, false)))	// "|" (instead of "||") ensures leave() is called even when rule==UNCONDITIONAL_WITH_STORAGE
		{
			hideField(); // the field will be left, so hide it
			return true;
		}
		else
			return false;
	}
	
	/**
	 * Handles request to leave the field.
	 * 
	 * @param record
	 * @param skipValidation skip validation if true (use with care!)
	 * @return whether or not leaving the field is allowed
	 */
	protected abstract boolean leave(Record record, boolean skipValidation);
	
	/**
	 * Checks whether the field, or rather the value that is (about to be) assigned, is valid.
	 * 
	 * @param record
	 * @return
	 */
	public abstract boolean isValid(Record record);
	
	/**
	 * @return whether or not the FieldUI is currently being shown as part of a page
	 */
	protected boolean isShownOnPage()
	{
		return shown && controller.getCurrentField() instanceof Page && collectorUI.getCurrentFieldUI() instanceof PageUI;
	}
	
	/**
	 * @return whether or not the FieldUI is currently being shown (possibly as part of a page)
	 */
	public boolean isFieldShown()
	{
		return shown;
	}
	
	/**
	 * When called the fieldUI is given the change to take the screen focus.
	 * If it does, it should return {@code true}, if it does not it should
	 * return {@code false}, which is also the default behaviour, but some
	 * subclasses will override this.
	 * 
	 * @return
	 */
	public boolean claimFocus()
	{
		return false;
	}

	/**
	 * Slightly hackish method to trigger (re)validation a fieldUI through the page that contains it.
	 * If the field is not a page its own validation method is used directly.
	 */
	@SuppressWarnings("unchecked")
	protected boolean isValidInformPage(Record record)
	{
		if(isShownOnPage())
			return ((PageUI<V, UI>) collectorUI.getCurrentFieldUI()).isValid(this, record);
		else
			return this.isValid(record); // validate field on its own
	}
	
	/**
	 * Slightly hackish way to remove the invalid mark (red border) around a field on a page.
	 * If the field is not on a page nothing happens.
	 */
	@SuppressWarnings("unchecked")
	protected void clearPageInvalidMark()
	{
		if(isShownOnPage())
			((PageUI<V, UI>) collectorUI.getCurrentFieldUI()).clearInvalidity(this);
	}
	
	public ControlsUI.State getControlState(Control control)
	{
		// Check if the field allows this control to be shown in the current formMode:
		boolean show = field.isControlAllowedToBeShown(control, controller.getCurrentMode());
		
		// Additional checks (if not forbidden by field):
		if(show)
			switch(control)
			{
				case BACK:
					show &= controller.canGoBack(false); // can we go back to a previous field or form
					break;
				case CANCEL:
					show &= isShowCancel();
					break;
				case FORWARD:
					show &= isShowForward();
					break;
			}
		
		// Return state
		return show ? State.SHOWN_ENABLED : State.HIDDEN; // for now we don't use SHOWN_DISABLED (= "grayed-out")
	}
	
	/**
	 * Whether or not to show the Cancel control above this fieldUI
	 * May be overridden (e.g. by {@link PageUI}).
	 *  
	 * @return
	 */
	protected boolean isShowCancel()
	{
		return controller.canGoBack(true); // can we go back within the current form
	}
	
	/**
	 * @return whether of not to show the Forward control above this fieldUI
	 */
	protected abstract boolean isShowForward();

	/**
	 * Tells whether this FieldUI needs to have its onDisplay() method called when it appears on the screen.
	 * 
	 * @param withPage whether the field is being display as part of a page (true) or on its own (false)
	 * @return whether or not onDisplay() must be called when the FieldUI is displayed to the user
	 */
	public /*final except for PageUI*/ boolean informOnDisplay(boolean withPage)
	{
		return isUsingAudioFeedback(withPage) || informOnDisplayNonAudioFeedback(withPage);
	}
	
	/**
	 * Behaviour to be executed when the FieldUI is displayed on the screen.
	 * Only called when informOnDisplay(boolean) returned true (for the same withPage value). 
	 * 
	 * @param withPage whether the field is being display as part of a page (true) or on its own (false)
	 */
	public /*final except for PageUI*/ void onDisplay(boolean withPage)
	{
		// Audio feedback:
		if(isUsingAudioFeedback(withPage))
			// Play (sequence of) audio feedback jobs:
			collectorUI.getAudioFeebackController().play(getAudioFeedbackJobs(field.form.getAudioFeedback(), withPage));
		// Other onDisplay behaviour:
		if(informOnDisplayNonAudioFeedback(withPage))
			onDisplayNonAudioFeedback(withPage);
	}
	
	/**
	 * Whether or not the field (and the form its part of) use audio feedback. 
	 * 
	 * @param withPage
	 * @return
	 */
	public final boolean isUsingAudioFeedback(boolean withPage)
	{
		return field.form.isUsingAudioFeedback() && isFieldUsingAudioFeedback(withPage);
	}
	
	/**
	 * To be overridden - always together with getAudioFeedbackJobs()! - in subclasses that support field-specific audio feedback. 
	 * This method is only called if the form the field is on uses audio feedback, so when this method is overridden that should not be checked again.
	 * 
	 * @param withPage whether the field is being display as part of a page (true) or on its own (false)
	 * @return whether the concrete FieldUI implementation supports/uses audio feedback
	 */
	protected boolean isFieldUsingAudioFeedback(boolean withPage)
	{
		return false; // not using audio feedback by default
	}
	
	/**
	 * To be overridden - always together with isFieldUsingAudioFeedback(boolean)! - in subclasses that support field-specific audio feedback.
	 * 
	 * @param audioFeedbackMode the audioFeedback mode of the form
	 * @param withPage whether the field is being display as part of a page (true) or on its own (false)
	 * @return
	 */
	protected List<AudioFeedbackController<V>.PlaybackJob> getAudioFeedbackJobs(AudioFeedback audioFeedbackMode, boolean withPage)
	{
		return Collections.<AudioFeedbackController<V>.PlaybackJob> emptyList(); // no jobs by default
	}
	
	/**
	 * To be overridden - always together with onDisplayNonAudioFeedback(boolean) - in subclasses that may need to
	 * execute additional, non-audiofeeback, behaviour upon the displaying of the fieldUI on the screen.
	 * 
	 * @param withPage whether the field is being display as part of a page (true) or on its own (false)
	 * @return
	 */
	protected boolean informOnDisplayNonAudioFeedback(boolean withPage)
	{
		return false; // inform not needed by default
	}
	
	/**
	 * To be overridden - always together with informOnDisplayNonAudioFeedback(boolean) - in subclasses that may need to
	 * execute additional, non-audiofeeback, behaviour upon the displaying of the fieldUI on the screen.
	 * 
	 * @param withPage
	 */
	protected void onDisplayNonAudioFeedback(boolean withPage)
	{
		// does nothing by default
	}
	
}
