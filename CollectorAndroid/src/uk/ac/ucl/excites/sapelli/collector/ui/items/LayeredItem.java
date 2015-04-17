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

import java.util.ArrayList;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.widget.RelativeLayout;

/**
 * A composite Item subclass which can layer multiple Items on top of each other
 * 
 * @author mstevens
 */
public class LayeredItem extends Item<LayeredItem>
{

	private ArrayList<Item<?>> layers;
	
	public LayeredItem()
	{
		this(null);
	}
	
	public LayeredItem(Integer id)
	{
		super(id);
		this.layers = new ArrayList<Item<?>>();
	}
	
	
	/**
	 * Adds a layer, newly added layers will be rendered on top of previously added ones.
	 * 
	 * Make sure the added layer item has:
	 * 	- an appropriate background colour:
	 * 		typically this will be a (semi-)transparent colour such that lower layers and/or
	 * 		the background colour of the LayeredItem itself remain (partially) visible;
	 * 	- an appropriate padding value:
	 * 		typically this will be 0, such that only the padding of the LayeredItem itself will appear.
	 * 
	 * @param layer the layer to add
	 * @return the LayeredItem itself
	 */
	public LayeredItem addLayer(Item<?> layer)
	{
		return addLayer(layer, layer.backgroundColor, layer.paddingDip); // keep layer background colour & padding as is
	}
	
	/**
	 * Adds a layer, newly added layers will be rendered on top of previously added ones.
	 * The layer's backgroundColor will be set to the given value.
	 * 
	 * @param layer the layer Item to add
	 * @param backgroundColor background colour to apply to layer
	 * @return the LayeredItem itself
	 */
	public LayeredItem addLayer(Item<?> layer, int backgroundColor)
	{
		return addLayer(layer, backgroundColor, layer.paddingDip); // keep layer padding as is
	}
	
	/**
	 * Adds a layer, newly added layers will be rendered on top of previously added ones.
	 * The layer's padding will be set to the given value.
	 * 
	 * @param layer the layer Item to add
	 * @param paddingDip padding to apply to layer
	 * @return the LayeredItem itself
	 */
	public LayeredItem addLayer(Item<?> layer, float paddingDip)
	{
		return addLayer(layer, layer.backgroundColor, paddingDip); // keep layer background colour as is
	}

	/**
	 * Adds a layer, newly added layers will be rendered on top of previously added ones.
	 * The layer's backgroundColor and padding will be set to the given values.
	 * 
	 * @param layer the layer Item to add
	 * @param backgroundColor background colour to apply to layer
	 * @param paddingDip padding to apply to layer
	 * @return the LayeredItem itself
	 */
	public LayeredItem addLayer(Item<?> layer, int backgroundColor, float paddingDip)
	{
		if(layer.backgroundColor != backgroundColor)
			layer.setBackgroundColor(backgroundColor); // use setter in case it is overridden
		if(layer.paddingDip != paddingDip)
			layer.setPaddingDip(paddingDip); // use setter in case it is overridden
		layers.add(layer);
		return this;
	}
	
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		RelativeLayout rl = new RelativeLayout(context);
		for(Item<?> layer : layers)
		{
			View layerView = layer.getView(context, recycleChildren);
			rl.addView(layerView, new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		}
		return rl;
	}

}
