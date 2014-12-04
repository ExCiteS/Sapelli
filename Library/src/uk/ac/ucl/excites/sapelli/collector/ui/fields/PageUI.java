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

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * @author mstevens
 *
 */
public abstract class PageUI<V, UI extends CollectorUI<V, UI>> extends FieldUI<Page, V, UI>
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
	public void hideField()
	{
		super.hideField(); // !!!
		// hide all contained fields:
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			if(controller.isFieldEnabled(fUI.getField())) // field is enabled (and shown)
				fUI.hideField(); // cancel() of each field will also be called
	}
	
	@Override
	protected void cancel()
	{
		// Disable triggers:
		controller.disableTriggers(field.getTriggers());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#leave(uk.ac.ucl.excites.sapelli.storage.model.Record, boolean)
	 */
	@Override
	protected boolean leave(Record record, boolean skipValidation)
	{
		if(skipValidation || isValid(record))
		{
			// Leave contained fields, but without repeating validation (values will be stored however):
			for(FieldUI<?, V, UI> fUI : fieldUIs)
				if(controller.isFieldEnabled(fUI.getField())) // field is enabled (and shown)
					fUI.leave(record, true); // storage only (no repeated validation)
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
				valid = false;								// don't return false here, some isValid() implementations may have side effects
		}
		return valid;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#informOnDisplay(boolean)
	 */
	@Override
	public boolean informOnDisplay(boolean withPage)
	{
		// Page itself:
		if(super.informOnDisplay(false))
			return true;
		// Children:
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			if(fUI.informOnDisplay(true))
				return true;
		return false;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#onDisplay(boolean)
	 */
	@Override
	public void onDisplay(boolean withPage)
	{
		// Page itself:
		super.onDisplay(false);
		// Children:
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			if(fUI.informOnDisplay(true))
				fUI.onDisplay(true);
	}
	
	/**
	 * Validated a specific field contained within the page
	 * 
	 * @param fUI
	 * @param record
	 * @return
	 */
	protected boolean isValid(FieldUI<?, V, UI> fUI, Record record)
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#claimFocus()
	 */
	@Override
	public boolean claimFocus()
	{
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			if(	controller.isFieldEnabled(fUI.getField())	// field is enabled (and shown)
				&& fUI.claimFocus())
				return true; // the first child field to claim the focus gets it.
		return false;
	}
	
	/**
	 * Overridden such that the cancel control is always shown (even if the page is the first field in the form; i.e. there is no field history).
	 * This will still be overruled if showCancelOnX=false was specified in XML though.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowCancel()
	 */
	@Override
	public boolean isShowCancel()
	{
		return true;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isShowForward()
	 */
	@Override
	protected boolean isShowForward()
	{
		return true;
	}
	
	@Override
	public void onLocationChanged(Location location)
	{
		if(location == null)
			return;
		for(FieldUI<?, V, UI> fUI : fieldUIs)
			if(controller.isFieldEnabled(fUI.getField())) // field is enabled (and shown)
				fUI.onLocationChanged(location);
	}

}
