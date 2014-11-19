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

import uk.ac.ucl.excites.sapelli.collector.ui.FontFitTextView;
import uk.ac.ucl.excites.sapelli.collector.ui.FontFitTextView.FontSizeCoordinator;
import android.content.Context;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.widget.TextView;

/**
 * @author mstevens
 *
 */
public class TextItem extends Item
{

	static public final int DEFAULT_TEXT_COLOR = Color.BLACK;
	
	private String text;
	private int textColor;
	private FontSizeCoordinator fontSizeCoordinator;
	
	public TextItem(String text, FontSizeCoordinator fontSizeCoordinator)
	{
		this(null, text, DEFAULT_TEXT_COLOR, fontSizeCoordinator);
	}
	
	public TextItem(String text, int textColour, FontSizeCoordinator fontSizeCoordinator)
	{
		this(null, text, textColour, fontSizeCoordinator);
	}
	
	public TextItem(Integer id, String text, FontSizeCoordinator fontSizeCoordinator)
	{
		this(id, text, DEFAULT_TEXT_COLOR, fontSizeCoordinator);
	}
	
	public TextItem(Integer id, String text, int textColour, FontSizeCoordinator fontSizeCoordinator)
	{
		super(id);
		this.text = text;
		this.textColor = textColour;
		this.fontSizeCoordinator = fontSizeCoordinator;
	}
		
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		TextView txtView = new FontFitTextView(context, fontSizeCoordinator);
		txtView.setTextColor(textColor);
		txtView.setGravity(Gravity.CENTER_VERTICAL | Gravity.CENTER_HORIZONTAL);
		txtView.setIncludeFontPadding(false);
		//txtView.setTextAlignment(View.TEXT_ALIGNMENT_CENTER);
		txtView.setText(text);
		return txtView;
	}

}
