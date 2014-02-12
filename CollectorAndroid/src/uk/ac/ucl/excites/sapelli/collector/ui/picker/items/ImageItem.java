/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.picker.items;

import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

/**
 * @author mstevens
 *
 */
public abstract class ImageItem extends Item
{
	
	static public final boolean DEFAULT_KEEP_VECTOR_ASPECT_RATIO = true; 
	
	protected final boolean vectorBased;
	protected boolean keepVectorAspectRatio = DEFAULT_KEEP_VECTOR_ASPECT_RATIO;
	
	public ImageItem(boolean vectorBased)
	{
		this.vectorBased = vectorBased;
	}
	
	protected Context context;
	
	@Override
	protected View createView(Context context)
	{
		this.context = context;
		ImageView view = new ImageView(context);
		// Set image:
		setImage(view);
		// Set scaling (raster-based images are only scaled down, never up; vector-based ones can be scaled up or down):
		view.setScaleType(isVectorBased() ? (keepVectorAspectRatio ? ScaleType.FIT_CENTER : ScaleType.FIT_XY) : ScaleType.CENTER_INSIDE);
		return view;
	}
	
	protected abstract void setImage(ImageView view);
	
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
