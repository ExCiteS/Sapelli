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
 * Super class for FieldUIs that can "leave themselves", rather than requiring an external request for that.
 * It is assumed they will handle any validation and storage behaviour themselves _before_ the calling of leave().
 * However, there are cases in which it will still be possible to leave it by means of external request
 * (one obvious example is the pressing of the forward button on a field with optionality=ALWAYS).
 * Therefore, basic validation of stored values is repeated at the point of leaving, but storage itself is not!
 * 
 * Note: subclasses may override isValid() to perform more specific checks (or no check at all if that is appropriate).
 * 
 * @param <F>
 * @param <V>
 * @param <UI>
 * 
 * @author mstevens
 */
public abstract class SelfLeavingFieldUI<F extends Field, V, UI extends CollectorUI<V, UI>> extends FieldUI<F, V, UI>
{

	public SelfLeavingFieldUI(F field, Controller<UI> controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#leave(uk.ac.ucl.excites.sapelli.storage.model.Record, boolean)
	 */
	@Override
	protected boolean leave(Record record, boolean skipValidation)
	{
		return skipValidation || isValid(record); // no storage happens at this point! (except sometimes in ButtonUI)
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean isValid(Record record)
	{
		return field.isNoColumn() || field.isOptional() || field.getColumn().isValuePresent(record);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isShowForward()
	 */
	@Override
	protected boolean isShowForward()
	{
		return field.isOptional();
	}
	
}
