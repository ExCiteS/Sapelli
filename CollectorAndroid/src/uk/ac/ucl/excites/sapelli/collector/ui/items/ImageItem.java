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
import android.view.ViewGroup;
import android.webkit.WebView;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.RelativeLayout;

/**
 * @author mstevens, benelliott
 *
 */
public abstract class ImageItem extends Item
{
	
	static public final boolean DEFAULT_KEEP_VECTOR_ASPECT_RATIO = true; 
	
	protected final boolean vectorBased;
	protected final boolean animation;
	protected boolean keepVectorAspectRatio = DEFAULT_KEEP_VECTOR_ASPECT_RATIO;
	
	public ImageItem(Integer id, boolean vectorBased, boolean animation)
	{
		super(id);
		this.vectorBased = vectorBased;
		this.animation = animation;
	}
		
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		
		if (animation) {
			//Use a RelativeLayout to house the WebView so that it can be associated with the Adapter's
			//onItemClickListener method
			RelativeLayout rl = new RelativeLayout(context);
			
			WebView view = new WebView(context); //house animation in a WebView
			rl.addView(view);

			setAnim(view);
			
			//vertically centre gif
			RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams)view.getLayoutParams();
			lp.addRule(RelativeLayout.CENTER_IN_PARENT, RelativeLayout.TRUE);
			view.setLayoutParams(lp);

			//prevent focus on WebView so LL can be clicked
			rl.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);
			
			return rl;
		}
		
		//if just a still image:
		ImageView view = new ImageView(context);
		// Set image:
		setImage(view);
		// Set scaling (raster-based images are only scaled down, never up; vector-based ones can be scaled up or down):
		view.setScaleType(isVectorBased() ? (keepVectorAspectRatio ? ScaleType.FIT_CENTER : ScaleType.FIT_XY) : ScaleType.CENTER_INSIDE);
		return view;
	}
	
	protected abstract void setImage(ImageView view);
	
	protected abstract void setAnim(WebView view);
	
	/**
	 * 
	 * @return whether or not the image is vector-based (i.e. loaded from a SVG/SVGZ file)
	 */
	public boolean isVectorBased()
	{
		return vectorBased; 
	}
	
	/**
	 * 
	 * @return whether or not the image is an animation
	 */
	public boolean isAnimation()
	{
		return animation; 
	}


	/**
	 * @return the keepVectorAspectRatio
	 */
	public boolean isKeepVectorAspectRatio()
	{
		return keepVectorAspectRatio;
	}

	/**
	 * @param keepVectorAspectRatio the keepVectorAspectRatio to set
	 */
	public void setKeepVectorAspectRatio(boolean keepVectorAspectRatio)
	{
		this.keepVectorAspectRatio = keepVectorAspectRatio;
	}

}