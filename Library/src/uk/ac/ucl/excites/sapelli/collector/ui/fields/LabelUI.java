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
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;


/**
 * @author mstevens
 *
 */
public abstract class LabelUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<LabelField, V, UI>
{

	public LabelUI(LabelField labelField, Controller controller, UI collectorUI)
	{
		super(labelField, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(Record record)
	{
		// does nothing (labels never have a column)
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(Record record)
	{
		return true;
	}

}
