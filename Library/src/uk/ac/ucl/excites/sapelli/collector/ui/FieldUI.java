package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.State;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.PageUI;
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
	
	protected F field;
	protected Controller controller;
	protected UI collectorUI;
	
	private Record lastKnownRecord = null;
	
	public FieldUI(F field, Controller controller, UI collectorUI)
	{
		this.field = field;
		this.controller = controller;
		this.collectorUI = collectorUI;
	}
	
	public F getField()
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
	public V getPlatformView(boolean onPage, Record record)
	{
		// Check if record is new:
		boolean newRecord = (lastKnownRecord != record);
		
		// Remember record:
		lastKnownRecord = record;
		
		return getPlatformView(onPage, record, newRecord);
	}
	
	/**
	 * Returns a platform-specific UI element (e.g. an Android View instance),
	 * the object may be recycled but should be updated w.r.t. the provided record.
	 * 
	 * @param onPage
	 * @param record
	 * @param newRecord whether or not this is a new record
	 * @return
	 */
	protected abstract V getPlatformView(boolean onPage, Record record, boolean newRecord);
	
	/**
	 * To be overridden by FieldUIs that need to execute cancelling behaviour before disappearing off the screen
	 */
	public void cancel()
	{
		// does nothing by default!
	}
	
	/**
	 * @param record
	 * @return whether or not leaving the field is allowed
	 */
	public boolean leave(Record record)
	{
		return leave(record, false); // apply validation!
	}
	
	/**
	 * Request to leave the field.
	 * 
	 * @param record
	 * @param noValidation skip validation if true (use with care!)
	 * @return whether or not leaving the field is allowed
	 */
	public abstract boolean leave(Record record, boolean noValidation);
	
	/**
	 * Checks whether the field, or rather the value that is (about to be) assigned, is valid.
	 * 
	 * @param record
	 * @return
	 */
	public abstract boolean isValid(Record record);
	
	protected boolean isShownOnPage()
	{
		return controller.getCurrentField() instanceof Page && collectorUI.getCurrentFieldUI() instanceof PageUI;
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
		boolean show = field.isControlAllowToBeShown(control, controller.getCurrentFormMode());
		
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
	 *  
	 * @return whether of not to show the Forward control above this fieldUI
	 */
	protected abstract boolean isShowForward();
	
}
