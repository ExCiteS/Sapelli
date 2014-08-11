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
import android.content.res.Resources;
import android.util.Log;
import android.webkit.WebView;
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
	//static private final String GIF_SUFFIX = "_gif";
	
	private Resources resources;
	private int resourceID;
	
	public ResourceImageItem(Resources resources, int resourceId)
	{
		this(null, resources, resourceId);
	}
	
	@SuppressLint("DefaultLocale")
	public ResourceImageItem(Integer  id, Resources resources, int resourceId)
	{
		super(
				id,
				resources.getResourceEntryName(resourceId).toLowerCase().endsWith(SVG_SUFFIX),
				false //TODO do not currently allow animated resources (not usually appropriate)
				);
		this.resources = resources;
		this.resourceID = resourceId;
	}

	//@SuppressLint("NewApi")
	@Override
	protected void setImage(ImageView view)
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
	
	@Override
	protected void setAnim(WebView view) //will never be called
	{
		//TODO
//		try
//		{
//			String url = "android.resource://"+resources.getResourcePackageName(resourceID)+"/"+resources.getResourceTypeName(resourceID)+"/"+resources.getResourceEntryName(resourceID);
//			Log.d("RII URI",url);
//			String html = "<html><body><img src=\"" + url + "\" width=\"50px\" /></body></html>"; //fit image to window
//			Log.d("RII URI",html);
//			Log.d("Res",resources.getResourcePackageName(resourceID)+", "+resources.getResourceTypeName(resourceID)+", "+resources.getResourceEntryName(resourceID));
//			Log.d("Res",resources.getResourceName(resourceID));
//			view.loadDataWithBaseURL(null,html, "text/html","UTF-8", null);
//			view.setScrollbarFadingEnabled(false); //disable scrollbars
//			
//			view.setOnTouchListener(new View.OnTouchListener() {
//			    @SuppressLint("ClickableViewAccessibility")
//				@Override
//			    public boolean onTouch(View v, MotionEvent event) {
//			    	//override normal WebView touch behaviour and pass click up to parent RelativeLayout
//	
//			    	RelativeLayout container = (RelativeLayout)v.getParent(); //find RelativeLayout by traversing up the view hierarchy
//			    	AdapterView<?> grid = (AdapterView<?>)container.getParent(); //find AdapterView that holds it
//			    	
//			    	//Force the AdapterView's performItemClick on the Item corresponding to the animation
//			    	return grid.performItemClick(
//			    			container,
//			    			grid.getPositionForView(container),
//			    			grid.getItemIdAtPosition(grid.getPositionForView(container))
//			    			);	
//			    } 
//		}
//		catch(Exception e)
//		{
//			Log.e(TAG, "Could not load image from resource: android.resource://"+resources.getResourcePackageName(resourceID)+"/"+resourceID, e);
//		}
	}

}