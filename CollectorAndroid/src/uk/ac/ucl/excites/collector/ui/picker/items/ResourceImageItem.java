/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker.items;

import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

//import com.caverock.androidsvg.SVG;
//import com.caverock.androidsvg.SVGImageView;

import android.content.res.Resources;
import android.util.Log;
import android.widget.ImageView;

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
	
	public ResourceImageItem(Resources resources, int id)
	{
		super(resources.getResourceEntryName(id).toLowerCase().endsWith(SVG_SUFFIX));
		this.resources = resources;
		this.resourceID = id;
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
				//SVG svg = SVG.getFromResource(context, resourceID);
		        //view.setImageDrawable(new PictureDrawable(svg.renderToPicture()));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load image from resource", e);
		}
	}

}
