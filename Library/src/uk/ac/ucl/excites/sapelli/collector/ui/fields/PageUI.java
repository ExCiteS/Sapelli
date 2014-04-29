/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class PageUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<Page, V, UI>
{

	protected List<FieldUI<?, V, UI>> fieldUIs;
	
	public PageUI(Page page, Controller controller, UI collectorUI)
	{
		super(page, controller, collectorUI);
		fieldUIs = new ArrayList<FieldUI<?, V, UI>>();
		
		for(Field f : page.getFields())
			CollectionUtils.addIgnoreNull(fieldUIs, f.createUI(collectorUI));
	}
	
	@Override
	public void cancel()
	{
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			fUI.cancel();
	}
	
	@Override
	protected boolean leave(Record record, boolean noValidation)
	{
		if(noValidation || isValid(record))
		{
			// Page will be left (and not to go to one of its contained fields, because in that case leave() wouldn't have been called), so disable its triggers:
			controller.disableTriggers(field.getTriggers());
			// Leave contained fields, but without repeating validation (values will be stored however):
			for(FieldUI<?, V, UI> fUI : fieldUIs)
				if(controller.isFieldEnabled(fUI.getField())) // field is enabled (and shown)
					fUI.leaveField(record, true);
			// Allow leaving:
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean isValid(Record record)
	{
		boolean valid = true;
		for(FieldUI<?, V, UI> fUI : fieldUIs)
		{
			if(	controller.isFieldEnabled(fUI.getField())	// field is enabled (and shown),
				&& !isValid(fUI, record))					// but not valid
				valid = false;
		}
		return valid;
	}
	
	public boolean isValid(FieldUI<?, V, UI> fUI, Record record)
	{
		boolean valid = fUI.isValid(record);
		markValidity(fUI, valid); // highlight with red border if invalid, remove border (if it is there) if valid
		return valid; 
	}
	
	protected abstract void markValidity(FieldUI<?, V, UI> fieldUI, boolean valid);

	public void clearInvalidity(FieldUI<?, V, UI> fieldUI)
	{
		if(fieldUIs.contains(fieldUI))
			markValidity(fieldUI, true);
	}
	
	@Override
	protected void storeValue(Record record)
	{
		// does nothing (Pages have no column of their own)
	}

	/**
	 * Overridden such that the cancel control is always shown (even if the page is the first field in the form; i.e. there is no field history).
	 * This will still be overruled if showCancelOnX=false was specified in XML though.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isShowCancel()
	 */
	@Override
	public boolean isShowCancel()
	{
		return true;
	}

}
