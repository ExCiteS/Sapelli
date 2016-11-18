/**
 * Sapelli data collection platform: http://sapelli.org
 * <p>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.HtmlField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;

/**
 * @author Michalis Vitos
 */
public abstract class HtmlUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<HtmlField, V, UI>
{

	public HtmlUI(HtmlField field, CollectorController<UI> controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

	/**
	 * Go back in the history of this WebView.
	 */
	public abstract void goBack();

	/**
	 * Check if web view can go back
	 */
	public abstract boolean canGoBack();

}
