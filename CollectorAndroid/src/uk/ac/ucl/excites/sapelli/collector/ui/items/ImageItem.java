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
import android.support.v4.view.ViewCompat;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

/**
 * @author mstevens
 *
 */
public abstract class ImageItem<II extends ImageItem<II>> extends Item<II>
{
	
	static public final boolean DEFAULT_KEEP_VECTOR_ASPECT_RATIO = true; 
	
	protected final boolean vectorBased;
	protected boolean keepVectorAspectRatio = DEFAULT_KEEP_VECTOR_ASPECT_RATIO;
	
	public ImageItem(Integer id, boolean vectorBased)
	{
		super(id);
		this.vectorBased = vectorBased;
	}
		
	@Override
	protected View createView(Context context, boolean recycleChildren)
	{
		ImageView view = new ImageView(context);
		
		// Set scaling (raster-based images are only scaled down, never up; vector-based ones can be scaled up or down):
		view.setScaleType(isVectorBased() ? (keepVectorAspectRatio ? ScaleType.FIT_CENTER : ScaleType.FIT_XY) : ScaleType.CENTER_INSIDE);
		
		/* Disable h/w acceleration for vector (SVG) images
		 * Reason explained here:
		 *  - https://github.com/japgolly/svg-android/commit/a1a613b
		 *  - http://stackoverflow.com/q/10384613/1084488 */
		if(isVectorBased())
			ViewCompat.setLayerType(view, ViewCompat.LAYER_TYPE_SOFTWARE, null);

		// Set image:
		setImage(context, view);
		
		return view;
	}
	
	protected abstract void setImage(Context context, ImageView view);
	
	/**
	 * 
	 * @return whether or not the image is vector-based (i.e. loaded from a SVG/SVGZ file)
	 */
	public boolean isVectorBased()
	{
		return vectorBased; 
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
