package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;

/**
 * @author mstevens
 *
 * @param <F>
 * @param <V>
 */
public abstract class FieldUI<F extends Field, V>
{
	
	protected F field;
	protected Controller controller;
	protected CollectorUI<V> collectorUI;
	
	public FieldUI(F field, Controller controller, CollectorUI<V> collectorUI)
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
	public abstract V getPlatformView(boolean onPage, CollectorRecord record);
	
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
	public boolean leave(CollectorRecord record)
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
	public abstract boolean leave(CollectorRecord record, boolean noValidation);
	
	/**
	 * Checks whether the field, or rather the value that is (about to be) assigned, is valid.
	 * 
	 * @param record
	 * @return
	 */
	public abstract boolean isValid(CollectorRecord record);
	
}
