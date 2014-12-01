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

//import com.caverock.androidsvg.SVG;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Resources;
import android.util.Log;
import android.widget.ImageView;

import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * An ImageItem subclass for images stored as app resources
 * 
 * Important: 
 * 	Because Android offers no way to programmatically glean the file extension of resource files (see http://stackoverflow.com/a/9403524/1084488)
 * 	we need another mechanism to detect SVG(Z) files. Therefore we use the following convention: the names of SVG(Z) resources of should end with "_svg".
 * 	I.e. their complete filenames will look like this: image_svg.svg[z]  
 * 
 * @author mstevens
 */
public class ResourceImageItem extends ImageItem
{

	static private final String TAG = "ResourceImageItem";
	static private final String SVG_SUFFIX = "_svg";
	
	private Resources resources;
	private int resourceID;
	
	public ResourceImageItem(Resources resources, int resourceId)
	{
		this(null, resources, resourceId);
	}
	
	@SuppressLint("DefaultLocale")
	public ResourceImageItem(Integer  id, Resources resources, int resourceId)
	{
		super(id, resources.getResourceEntryName(resourceId).toLowerCase().endsWith(SVG_SUFFIX));
		this.resources = resources;
		this.resourceID = resourceId;
	}

	//@SuppressLint("NewApi")
	@Override
	protected void setImage(Context context, ImageView view)
	{
		try
		{
			if(!isVectorBased())
			{	// Raster image (PNG, JPG, GIF, ...):
				view.setImageResource(resourceID);
			}
			else
			{	// Vector image (SVG or SVGZ):
				
				// Using svg-android library:
				SVG svg = new SVGBuilder().readFromResource(resources, resourceID).build();
				view.setImageDrawable(new SVGDrawable(svg));
				
				// Usin AndroidSVG library:
				//view.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
				//SVG svg = SVG.getFromResource(resources, resourceID);
		        //view.setImageDrawable(new PictureDrawable(svg.renderToPicture()));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load image from resource", e);
		}
	}

}
