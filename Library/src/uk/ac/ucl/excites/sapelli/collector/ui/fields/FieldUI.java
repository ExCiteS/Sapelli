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
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Abstract class to represent the UI of a Field
 * 
 * @author mstevens, Ben
 *
 * @param <F>
 * @param <V>
 * @param <UI>
 */
public abstract class FieldUI<F extends Field, V, UI extends CollectorUI<V, UI>>
{
	
	static public final byte VISIBILITY_HIDDEN = 0;
	static public final byte VISIBILITY_SHOWN_ALONE = 1;
	static public final byte VISIBILITY_SHOWN_ON_PAGE = 2;
	
	public final F field;
	protected final Controller<UI> controller;
	protected final UI collectorUI;
	
	private boolean current = false;
	private int visibility = VISIBILITY_HIDDEN;
	private boolean newRecord = true;
	private Record lastKnownRecord = null;
	private boolean wasUserGoingBack = false;
	
	public FieldUI(F field, Controller<UI> controller, UI collectorUI)
	{
		this.field = field;
		this.controller = controller;
		this.collectorUI = collectorUI;
	}
	
	public final void update()
	{
		if(!current) // Only update if no longer current...
		{				
			// Set visibility base on Controller's current state:
			Field currentField = controller.getCurrentField();
			visibility = (currentField == field ?
							VISIBILITY_SHOWN_ALONE :
							(currentField instanceof Page && currentField == field.getPage() ?
								VISIBILITY_SHOWN_ON_PAGE :
								VISIBILITY_HIDDEN));
			
			// Update additional state variables if the field is/we be shown:
			if(visibility != VISIBILITY_HIDDEN)
			{
				// Remember whether this field was reached by means of a user-initiated back-press:
				wasUserGoingBack = controller.isGoingBack();

				// Record related state:
				Record currentRecord = controller.getCurrentRecord();
				//	Check if record is new:
				newRecord = (lastKnownRecord != currentRecord);
				//	Remember record:
				lastKnownRecord = currentRecord;
				
				// Set subclass-specific (and possibly argument-dependent) state: 
				update(currentRecord, visibility != VISIBILITY_SHOWN_ON_PAGE ? controller.getCurrentFieldArguments() : FieldParameters.EMPTY); // fields shown on a page never get arguments
			}
			
			// FieldUI is now uptodate:
			current = true;
		}
	}
	
	/**
	 * Subclass-specific update routines go here.
	 * 
	 * @param arguments
	 */
	protected void update(Record record, FieldParameters arguments)
	{
		// does nothing by default
	}
	
	/**
	 * Returns a platform-specific UI element (e.g. an Android View instance),
	 * the object may be recycled but should be updated w.r.t. the provided record.
	 * 
	 * @return
	 */
	public final V showField()
	{
		// Update FieldUI state if needed:
		if(!current)
			update(); // !!!
		
		// Just in case...
		if(!isFieldShown())
			throw new IllegalStateException("Field \"" + field.id + "\" cannot be shown because it is not (part of) the current field (\"" + controller.getCurrentField().id + "\")!");
		
		// Return platform-specific UI element:
		return getPlatformView(isFieldShownOnPage(), controller.isFieldEnabled(field), controller.getCurrentRecord(), newRecord);
	}
	
	/**
	 * Returns a platform-specific UI element (e.g. an Android View instance),
	 * the object may be recycled but should be updated w.r.t. the provided record.
	 * 
	 * @param onPage
	 * @param enabled
	 * @param record
	 * @param newRecord whether or not this is a new record
	 * @return
	 */
	protected abstract V getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord);
	
	/**
	 * Hides/cancels the fieldUI
	 */
	public final void hideField()
	{
		// Stop any audiofeedback which may still be running:
		if(isUsingAudioFeedback(isFieldShownOnPage()))
			collectorUI.stopAudioFeedback();
		
		// Mark fieldUI as *not* currently shown:
		this.visibility = VISIBILITY_HIDDEN;
		
		// This also means the FieldUI will need to be updated before it can be shown again:
		this.current = false;
		
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
	protected boolean isFieldShownOnPage()
	{
		return visibility == VISIBILITY_SHOWN_ON_PAGE;
		// Alternative: return controller.getCurrentField() instanceof Page && collectorUI.getCurrentFieldUI() instanceof PageUI;
	}
	
	public boolean isFieldShownAlone()
	{
		return visibility == VISIBILITY_SHOWN_ALONE;
	}
	
	/**
	 * @return whether or not the FieldUI is currently being shown (possibly as part of a page)
	 */
	public boolean isFieldShown()
	{
		return visibility != VISIBILITY_HIDDEN;
	}
	
	/**
	 * @return the wasGoingBack whether or not the currently displayed field was reached by means of a user-initiated back-press
	 */
	public boolean wasUserGoingBack()
	{
		return wasUserGoingBack;
	}

	/**
	 * @return the newRecord
	 */
	public boolean isNewRecord()
	{
		return newRecord;
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
		if(isFieldShownOnPage())
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
		if(isFieldShownOnPage())
			((PageUI<V, UI>) collectorUI.getCurrentFieldUI()).clearInvalidity(this);
	}
	
	/**
	 * @param control
	 * @return the current ControlsUI.State for the given Control.Type
	 */
	public ControlsUI.State getControlState(Control.Type control)
	{
		// Check if the field (as statically defined in the project XML) allows this control to be shown in the current formMode:
		boolean show = field.isControlAllowedToBeShown(control, controller.getCurrentMode());
		
		// If not forbidden by field/formMode, perform additional (dynamic) checks... 
		if(show)
		{
			// But first update the FieldUI state if needed:
			if(!current)
				update(); // !!!
			
			// Check...
			switch(control)
			{
				case Back:
					show &= isShowBack();
					break;
				case Cancel:
					show &= isShowCancel();
					break;
				case Forward:
					show &= isShowForward();
					break;
			}
		}
		
		// Return state
		return show ? ControlsUI.State.SHOWN_ENABLED : ControlsUI.State.HIDDEN; // for now we don't use SHOWN_DISABLED (= "grayed-out")
	}
	
	/**
	 * Whether or not to show the Back control above this fieldUI
	 * This should *only* be overridden if {@link #handleControlEvent(Control)} is also overridden to perform a custom "back action"
	 *  
	 * @return
	 */
	protected boolean isShowBack()
	{
		return controller.canGoBack(false); // can we go back to a previous field or form
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
	 * Allows subclasses to handle control events (i.e. pressing back/exit/forward)
	 * in ways specific to their UI.
	 * 
	 * @param control - the control that was pressed
	 * @return {@code true} if the control event was consumed and the default behaviour should not be
	 * enacted, {@code false} if the control event was not consumed and the default behaviour should
	 * be enacted.
	 */
	public boolean handleControlEvent(Control.Type control)
	{
		// by default, do not consume events:
	    return false;
    }

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
