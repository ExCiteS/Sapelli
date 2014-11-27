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

import uk.ac.ucl.excites.sapelli.collector.ui.FontFitSurfaceView;
import uk.ac.ucl.excites.sapelli.collector.ui.FontFitSurfaceView.TextSizeCoordinator;
import android.content.Context;
import android.graphics.Color;
import android.view.View;

/**
 * An Item with Text rendered at a font size that fit the item bounds, possibly coordinated across other items/views
 * 
 * @author mstevens
 */
public class TextItem extends Item
{

	static public final int DEFAULT_TEXT_COLOR = Color.BLACK;
	
	private String text;
	private int textColor;
	private TextSizeCoordinator textSizeCoordinator;
	private int coordinatorSlot = -1;
	
	/**
	 * TextItem with given text, default text colour, and uncoordinated text size
	 * 
	 * @param text
	 */
	public TextItem(String text)
	{
		this(null, text, DEFAULT_TEXT_COLOR, null);
	}
	
	/**
	 * TextItem with given text, default text colour, and text size coordinated by the given TextSizeCoordinator (unless it is null)
	 * 
	 * @param text
	 * @param textSizeCoordinator
	 */
	public TextItem(String text, TextSizeCoordinator textSizeCoordinator)
	{
		this(null, text, DEFAULT_TEXT_COLOR, textSizeCoordinator);
	}
	
	/**
	 * TextItem with given text, given text colour, and uncoordinated text size
	 * 
	 * @param text
	 * @param textColour
	 */
	public TextItem(String text, int textColour)
	{
		this(null, text, textColour, null);
	}
	
	/**
	 * TextItem with given text, given text colour, and text size coordinated by the given TextSizeCoordinator (unless it is null)
	 * 
	 * @param text
	 * @param textColour
	 * @param textSizeCoordinator
	 */
	public TextItem(String text, int textColour, TextSizeCoordinator textSizeCoordinator)
	{
		this(null, text, textColour, textSizeCoordinator);
	}
	
	/**
	 * TextItem with given id, given text, given text colour, and text size coordinated by the given TextSizeCoordinator (unless it is null)
	 * 
	 * @param id
	 * @param text
	 * @param textColour
	 * @param fontSizeCoordinator
	 */
	public TextItem(Integer id, String text, int textColour, TextSizeCoordinator fontSizeCoordinator)
	{
		super(id);
		this.text = text;
		this.textColor = textColour;
		this.textSizeCoordinator = fontSizeCoordinator;
		if(fontSizeCoordinator != null)
			this.coordinatorSlot = fontSizeCoordinator.claimSlot();
	}
	
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		FontFitSurfaceView txtView = new FontFitSurfaceView(context, textSizeCoordinator, coordinatorSlot);
		txtView.setTextColor(textColor);
		txtView.setText(text);
		return txtView;
	}

}
