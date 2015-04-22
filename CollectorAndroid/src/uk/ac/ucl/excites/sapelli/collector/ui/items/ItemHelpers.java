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

package uk.ac.ucl.excites.sapelli.collector.ui.items;

import android.graphics.Color;

/**
 * @author mstevens
 */
public final class ItemHelpers
{

	private ItemHelpers() {}

	static public final int COLOR_SEMI_TRANSPARENT_GRAY = Color.parseColor("#CC777777");
	
	static public LayeredItem GrayOut(Item<?> item)
	{
		return new LayeredItem()
		.setPaddingDip(0.0f)
		//	Add content:
		.addLayer(item)
		//	Add grayed-out layer on top:
		.addLayer(new EmptyItem(), COLOR_SEMI_TRANSPARENT_GRAY, 0.0f);
	}
	
}
