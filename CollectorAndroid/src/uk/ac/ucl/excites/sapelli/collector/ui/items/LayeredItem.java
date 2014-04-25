/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.items;

import java.util.ArrayList;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.widget.RelativeLayout;

/**
 * A composite Item subclass which can layer multiple Items on top of each other
 * 
 * @author mstevens
 */
public class LayeredItem extends Item
{

	private ArrayList<Item> layers;
	
	public LayeredItem()
	{
		this(null);
	}
	
	public LayeredItem(Long id)
	{
		super(id);
		this.layers = new ArrayList<Item>();
	}
	
	/**
	 * Add a layer, newly added layers will be rendered on top of previously added ones 
	 * 
	 * @param item
	 */
	public void addLayer(Item item)
	{
		addLayer(item, true); // make background transparent by default
	}
	
	/**
	 * Add a layer, newly added layers will be rendered on top of previously added ones 
	 * 
	 * @param item
	 * @param transparentBackground	whether or not to make the layer's background transparent
	 */
	public void addLayer(Item item, boolean transparentBackground)
	{
		if(transparentBackground)
			item.setBackgroundColor(Color.TRANSPARENT);
		layers.add(item);
	}
	
	@Override
	protected View createView(Context context)
	{
		RelativeLayout rl = new RelativeLayout(context);
		for(Item layer : layers)
		{
			View layerView = layer.getView(context);
			rl.addView(layerView, new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		}
		return rl;
	}

}
