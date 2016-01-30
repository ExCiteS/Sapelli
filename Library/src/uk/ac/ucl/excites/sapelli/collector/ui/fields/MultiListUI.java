/**
 * Sapelli data collection platform: http://sapelli.org
 *
 * Copyright 2012-2016 University College London - ExCiteS group
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

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField.MultiListItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * FieldUI for MultiList fields
 *
 * TODO maybe allow list navigation/selection by keystrokes (in which case the keyboard should be hidden in cancel())
 *
 * @author mstevens
 */
public abstract class MultiListUI<V, UI extends CollectorUI<V, UI>> extends NonSelfLeavingFieldUI<MultiListField, V, UI>
{

	public MultiListUI(MultiListField listField, CollectorController<UI> controller, UI collectorUI)
	{
		super(listField, controller, collectorUI);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI#storeValue(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	protected void storeValue(Record record)
	{
		MultiListItem chosen = getChosenItem();
		if(!field.isNoColumn())
			field.getColumn().storeValue(record, chosen != null ? field.getValueForItem(chosen) : null);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(Record record)
	{
		MultiListItem chosen = getChosenItem();
		return chosen == null ? field.isOptional() : chosen.isLeaf();
	}

	protected abstract MultiListItem getChosenItem();

}
