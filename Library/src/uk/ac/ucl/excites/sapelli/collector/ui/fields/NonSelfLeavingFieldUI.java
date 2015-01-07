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

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Superclass for FieldUIs that can only be left due to external request (e.g. the forward button being pressed) and
 * therefore need to execute validation and data storage behaviour at that moment.
 * 
 * @param <F>
 * @param <V>
 * @param <UI>
 * 
 * @author mstevens
 */
public abstract class NonSelfLeavingFieldUI<F extends Field, V, UI extends CollectorUI<V, UI>> extends FieldUI<F, V, UI>
{

	public NonSelfLeavingFieldUI(F field, Controller<UI> controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/**
	 * Unless noValidation=true, leaving is only allowed upon successful validation, and if valid the value must first be stored.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#leave(Record, boolean)
	 */
	@Override
	protected boolean leave(Record record, boolean skipValidation)
	{
		if(skipValidation || isValid(record))
		{
			if(!field.isNoColumn())
			{
				try
				{
					storeValue(record);
				}
				catch(Exception e)
				{
					e.printStackTrace(System.err);
					controller.addLogLine("STORAGE ERROR", e.getClass().getName(), (e.getMessage() != null ? e.getMessage() : ""));
					return skipValidation;
				}
			}
			return true;
		}
		return false;
	}
	
	/**
	 * It can safely assumed that the field#noColumn=false (except in the case of Pages) and that isValid() has been called beforehand and returned true.
	 * 
	 * @param record
	 * @throws Exception 
	 */
	protected abstract void storeValue(Record record) throws Exception;

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isShowForward()
	 */
	@Override
	protected boolean isShowForward()
	{
		return true;
	}
	
}
