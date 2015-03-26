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

import uk.ac.ucl.excites.sapelli.collector.ui.MeasureView;
import uk.ac.ucl.excites.sapelli.collector.ui.TextFitView;
import uk.ac.ucl.excites.sapelli.collector.ui.TextFitView.TextSizeCoordinator;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.View;

/**
 * An Item that displays the dimensions (W * H) it is being given on the screen.
 * MeasureItems always have 0 padding.
 * 
 * Usage: use one of the static Measure() method to wrap an Item that needs measuring
 * in a LayeredItem along with MeasureItem on top of it. 
 * 
 * @author mstevens
 */
public class MeasureItem extends Item<MeasureItem>
{

	// STATICS ------------------------------------------------------
	/**
	 * Default text colour is white
	 */
	static public final int DEFAULT_TEXT_COLOR = Color.WHITE;
	
	/**
	 * Default background colour is 1/3 transparent red
	 */
	static public final int DEFAULT_BACKGROUND_COLOR = Color.argb(255/3, 255, 0, 0);

	/**
	 * @param content the Item to measure
	 * @return a LayeredItem containing the given content and a MeasureItem on top of it 
	 * @see #Measure(Item, int, TextSizeCoordinator)
	 */
	static public LayeredItem Measure(Item<?> content)
	{
		return Measure(content, DEFAULT_TEXT_COLOR, DEFAULT_BACKGROUND_COLOR, null);
	}
	
	/**
	 * @param content the Item to measure
	 * @param textSizeCoordinator if not null this TextSizeCoordinator will be used to determine/coordinate the font size on the MeasureItem(s)
	 * @return a LayeredItem containing the given content and a MeasureItem on top of it 
	 * @see #Measure(Item, int, TextSizeCoordinator)
	 */
	static public LayeredItem Measure(Item<?> content, TextSizeCoordinator textSizeCoordinator)
	{
		return Measure(content, DEFAULT_TEXT_COLOR, DEFAULT_BACKGROUND_COLOR, textSizeCoordinator);
	}
	
	/**
	 * @param content the Item to measure
	 * @param textColour the colour of the text on the MeasureItem
	 * @param backgroundColor the background colour of MeasureItem, use a (semi-)transparent colour when the content layer needs to remain (partially) visible
	 * @return a LayeredItem containing the given content and a MeasureItem on top of it
	 * @see #Measure(Item, int, TextSizeCoordinator)
	 */
	static public LayeredItem Measure(Item<?> content, int textColour, int backgroundColor)
	{
		return Measure(content, textColour, backgroundColor, null);
	}
	
	/**
	 * Creates a new LayeredItem containing the given content Item and a MeasureItem on top of it.
	 * The background colour of the content layer is made transparent and its padding is made 0, instead
	 * the original background colour and padding are applied to the container (i.e. the LayeredItem).
	 * By MeasureItem layer has 0 padding, therefore matching the dimensions of the content item being measured.
	 * 
	 * @param content the Item to measure
	 * @param textColor the colour of the text on the MeasureItem
	 * @param backgroundColor the background colour of MeasureItem, use a (semi-)transparent colour when the content layer needs to remain (partially) visible
	 * @param textSizeCoordinator if not null this TextSizeCoordinator will be used to determine/coordinate the font size on the MeasureItem(s) 
	 * @return a LayeredItem containing the given content and a MeasureItem on top of it
	 * 
	 *  // this way the displayed dimensions will match the wrapped Item's clipped bounds (i.e. measured within the padding it would have had if not being wrapped)
	 */
	static public LayeredItem Measure(Item<?> content, int textColor, int backgroundColor, TextSizeCoordinator textSizeCoordinator)
	{
		return new LayeredItem(content.id)
			// use content background colour as container background colour:
			.setBackgroundColor(content.backgroundColor)
			// use content padding as container padding:
			.setPaddingDip(content.paddingDip)
			// add content as bottom layer to container ...
			.addLayer(content, Color.TRANSPARENT, 0.0f) // (make content background transparent and remove its padding)
			// add MeasureItem as top layer:
			.addLayer(new MeasureItem(null, textColor, backgroundColor, textSizeCoordinator)); // (has 0 padding)
	}

	// DYNAMICS -----------------------------------------------------
	private int textColor;
	private TextSizeCoordinator textSizeCoordinator;
	private int coordinatorSlot = TextFitView.UNASSIGNED_SLOT;
	
	/**
	 * TextItem with given id, given text colour, and text size coordinated by the given TextSizeCoordinator (unless it is null)
	 * 
	 * @param id
	 * @param textColor the colour of the measurement text
	 * @param backgroundColour the background colour, use a (semi-)transparent colour when using the MeasureItem in a LayeredItem in which lower layer(s) need to remain (partially) visible
	 * @param textSizeCoordinator
	 */
	private MeasureItem(Integer id, int textColor, int backgroundColor, TextSizeCoordinator textSizeCoordinator)
	{
		super(id);
		this.textColor = textColor;
		this.backgroundColor = backgroundColor;
		this.textSizeCoordinator = textSizeCoordinator;
		if(textSizeCoordinator != null)
			this.coordinatorSlot = textSizeCoordinator.claimSlot();		
		this.paddingDip = 0.0f; // remove padding
	}
	
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		MeasureView mView = new MeasureView(context, textSizeCoordinator, coordinatorSlot);
		mView.setTextColor(textColor);
		return mView;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.items.Item#setPaddingDip(float)
	 */
	@Override
	public MeasureItem setPaddingDip(float paddingDip)
	{
		Log.d(getClass().getSimpleName(), "Ignoring call to setPaddingDip().");
		return this;
	}

}
