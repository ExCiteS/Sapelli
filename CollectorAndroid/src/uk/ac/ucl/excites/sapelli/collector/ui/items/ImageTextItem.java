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

import java.io.File;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;

/**
 * A composite Item subclass which displays an image with a caption below it
 * 
 * @author benelliott
 */
public class ImageTextItem extends Item
{
	private FileImageItem img; //FileImageItem displays the Choice's image
	private TextItem text; //TextItem displays the Choice's altText
	private float altHeight; //decimal representing proportion of item's height to be taken by the TextItem
	
	public ImageTextItem(File imageFile, String altText, float altHeight) {
		this(null, imageFile, altText, altHeight);
	}
	
	public ImageTextItem(Integer id, File imageFile, String altText, float altHeight) {
		super(id);
		img = new FileImageItem(imageFile); //create FileImageItem from File object
		text = new TextItem(altText); //create TextItem from altText String
		this.altHeight = altHeight;
	}
	
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		LinearLayout ll = new LinearLayout(context); //instantiate a LinearLayout to hold the items
		ll.setOrientation(LinearLayout.VERTICAL);
		ll.setWeightSum(1f);
		//Obtain image's View, set weight accordingly, then add to LinearLayout
		View imgView = img.getView(context, recycleChildren);
		ll.addView(imgView);
		LinearLayout.LayoutParams lllp = new LinearLayout.LayoutParams(
				LinearLayout.LayoutParams.MATCH_PARENT,
				0,
				1f - altHeight //altHeight determines height of text box
				);

		//Log.d("ImageTextItem","weight: "+lllp.weight);
		imgView.setLayoutParams(lllp);
		
		//Obtain text box View, set weight accordingly, then add to LinearLayout
		View textView = text.getView(context, recycleChildren);
		lllp = new LinearLayout.LayoutParams(
				LinearLayout.LayoutParams.MATCH_PARENT,
				0,
				altHeight
				);
		textView.setLayoutParams(lllp);
		ll.addView(textView);
		return ll;
	}
}
