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

package uk.ac.ucl.excites.sapelli.collector.control;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Helper class which holds all state variables needed to manage an open "form session"
 * 
 * @author mstevens
 */
public class FormSession
{
	
	// STATIC -------------------------------------------------------
	static public FormSession Create(Form form, Controller controller)
	{
		return new FormSession(form, Mode.CREATE, form.isProducesRecords() ? form.newRecord(controller.getDeviceID()) : null, controller.getElapsedMillis());
	}
	
	static public FormSession Edit(Form form, Record record, Controller controller)
	{
		return new FormSession(form, Mode.EDIT, record, controller.getElapsedMillis());
	}
	
	// DYNAMIC ------------------------------------------------------
	protected final Form form;
	protected final Mode mode;
	protected Record record; //TODO make final? do we really need to make the record null in Controller#discardRecordAndAttachments()?
	protected final long startTime;
	
	private final Stack<FieldWithArguments> fieldAndArgumentHistory;
	private FieldWithArguments currFieldAndArguments = null;
	private boolean currFieldDisplayed = false;
	private Map<Field,Boolean> runtimeEnabled = null; // only instantiated when needed
	
	/**
	 * @param form
	 * @param mode
	 * @param record
	 */
	private FormSession(Form form, Mode mode, Record record, long startTime)
	{
		if(form == null)
			throw new NullPointerException("Form cannot be null!");
		if(record == null && form.isProducesRecords())
			throw new NullPointerException("Record cannot be null because this is a record-producing form!");	
		this.form = form;
		this.mode = mode;
		this.record = record;
		this.fieldAndArgumentHistory = new Stack<FieldWithArguments>();
		this.startTime = startTime;
	}
	
	public FieldWithArguments getPrevious(boolean forBackMove)
	{
		for(int f = fieldAndArgumentHistory.size() - 1; f >= 0; f--)
		{	// loop through stack from top to bottom:
			FieldWithArguments fwA = fieldAndArgumentHistory.get(f);
			if(!forBackMove || !fwA.field.isSkipOnBack())
				return fwA;
		}
		return null;
	}
	
	public boolean canGoBack()
	{
		return getPrevious(true) != null;
	}
	
	/**
	 * Called from {@link Controller#goTo(FieldWithArguments, boolean)}
	 * 
	 * @param nextFieldAndArguments
	 */
	public void setCurrent(FieldWithArguments nextFieldAndArguments)
	{
		// Check if we already visited the next field (i.e. either because we are handling a goBack(), or a jump to a field that was visited before):
		boolean alreadyVisited = false;
		for(FieldWithArguments fwA : fieldAndArgumentHistory) // Note: stack iterates from bottom up
			if(fwA.field == nextFieldAndArguments.field)
				alreadyVisited = true;
		if(alreadyVisited)
			// Revert if already visited (remove previous visit and all fields "above"/since it)
			while(fieldAndArgumentHistory.pop().field != nextFieldAndArguments.field) {}
		else if(atField() && nextFieldAndArguments.field != currFieldAndArguments.field)
			// If the next field was not already visited *and* we are currently at a field *and* that field is not the same as the next one...
			fieldAndArgumentHistory.push(currFieldAndArguments); // then add the current field to the history.
		
		// 	Next becomes the (new) current...
		this.currFieldAndArguments = nextFieldAndArguments;
	}
	
	/**
	 * @return the currFieldDisplayed
	 */
	public boolean isCurrentFieldDisplayed()
	{
		return currFieldDisplayed;
	}
	
	/**
	 * Set the "runtime enabledness" of the given field
	 * 
	 * @param field
	 * @param enabled
	 */
	protected void setRuntimeEnabled(Field field, boolean enabled)
	{
		if(runtimeEnabled == null)
			runtimeEnabled = new HashMap<Field, Boolean>();
		runtimeEnabled.put(field, enabled);
	}
	
	/**
	 * Get the "runtime enabledness" of the given field (often null)
	 * 
	 * @param field
	 * @return
	 */
	protected Boolean getRuntimeEnabled(Field field)
	{
		return runtimeEnabled != null ? runtimeEnabled.get(field) : null;
	}
	
	/**
	 * @param currFieldDisplayed the currFieldDisplayed to set
	 */
	protected void setCurrentFieldDisplayed(boolean currFieldDisplayed)
	{
		this.currFieldDisplayed = currFieldDisplayed;
	}

	public FieldWithArguments getCurrent()
	{
		return currFieldAndArguments;
	}
	
	public Field getCurrentField()
	{
		return currFieldAndArguments != null ? currFieldAndArguments.field : null;
	}
	
	public FieldParameters getCurrentFieldArguments()
	{
		return currFieldAndArguments != null ? currFieldAndArguments.arguments : null;
	}
	
	public boolean atField()
	{
		return currFieldAndArguments != null;
	}	
}
