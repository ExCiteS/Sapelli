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

import android.content.Context;
import android.view.View;

/**
 * @author mstevens
 *
 */
public class EmptyItem extends Item<EmptyItem>
{
	
	public EmptyItem()
	{
		this(null);
	}
	
	public EmptyItem(Integer id)
	{
		super(id);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item#createView(android.content.Context)
	 */
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		return new View(context);
	}

}
