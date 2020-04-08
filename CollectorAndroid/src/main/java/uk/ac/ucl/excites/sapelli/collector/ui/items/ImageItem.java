/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import java.io.FileInputStream;

import com.caverock.androidsvg.SVG;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.drawable.PictureDrawable;
import androidx.core.view.ViewCompat;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.media.MediaHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.BitmapUtils;

/**
 * An {@link Item} which displays an image loaded from a file or a drawable resource.
 * 
 * @author mstevens
 */
public class ImageItem extends Item<ImageItem>
{

	// STATIC -----------------------------------------------------------------
	static private final String TAG = ImageItem.class.getSimpleName();

	static public final boolean DEFAULT_KEEP_VECTOR_ASPECT_RATIO = true; 

	/**
	 * Returns a new ImageItem based on an image file, or, if the given file is null/doesn't exist/isn't readable, a drawable resource.
	 * 
	 * @param file an image file, may be null provided that {@code resources} and {@code drawableResourceId} are *not*
	 * @param resources a {@link Resources} object which provides access to a drawable resources with id {@code drawableResourceId}, may be {@code null} provided that {@code file} is *not* 
	 * @param drawableResourceId the id of a drawable that is accessible through {@code resources}, may be {@code null} provided that {@code file} is *not*
	 * @return an ImageItem based on either an image file or a drawable resource
	 */
	static public ImageItem New(File file, Resources resources, Integer drawableResourceId)
	{
		return New(null, file, resources, drawableResourceId);
	}

	/**
	 * Returns a new ImageItem based on an image file, or, if the given file is null/doesn't exist/isn't readable, a drawable resource.
	 * 
	 * @param id the {@link Item} id (may be null)
	 * @param imageFile an image file, may be null provided that {@code resources} and {@code drawableResourceId} are *not*
	 * @param resources a {@link Resources} object which provides access to a drawable resources with id {@code drawableResourceId}, may be {@code null} provided that {@code file} is *not* 
	 * @param drawableResourceId the id of a drawable that is accessible through {@code resources}, may be {@code null} provided that {@code file} is *not*
	 * @return an ImageItem based on either an image file or a drawable resource
	 */
	static public ImageItem New(Integer id, File imageFile, Resources resources, Integer drawableResourceId)
	{
		if(FileHelpers.isReadableFile(imageFile))
			return new ImageItem(id, imageFile);
		else
			return new ImageItem(id, resources, drawableResourceId.intValue());
	}

	// DYNAMIC ----------------------------------------------------------------
	private final File file;
	private final int drawableResourceID;

	protected final boolean vectorBased;
	protected boolean keepVectorAspectRatio = DEFAULT_KEEP_VECTOR_ASPECT_RATIO;

	/**
	 * Creates a new ImageItem based on an image file.
	 * 
	 * @param file an image file
	 */
	public ImageItem(File file)
	{
		this(null, file);
	}

	/**
	 * Creates a new ImageItem based on an image file.
	 * 
	 * @param id the {@link Item} id (may be null)
	 * @param file an image file
	 */
	public ImageItem(Integer id, File file)
	{
		super(id);
		this.drawableResourceID = -1;
		this.file = file;
		this.vectorBased = MediaHelpers.isVectorImageFileName(file.getName());
	}

	/**
	 * Creates a new ImageItem based on a drawable resource.
	 * 
	 * @param resources a {@link Resources} object which provides access to a drawable resources with id {@code drawableResourceId} 
	 * @param drawableResourceId the id of a drawable that is accessible through {@code resources}
	 */
	public ImageItem(Resources resources, int drawableResourceId)
	{
		this(null, resources, drawableResourceId);
	}

	/**
	 * Creates a new ImageItem based on a drawable resource.
	 * 
	 * @param id the {@link Item} id (may be null)
	 * @param resources a {@link Resources} object which provides access to a drawable resources with id {@code drawableResourceId} 
	 * @param drawableResourceId the id of a drawable that is accessible through {@code resources}
	 */
	public ImageItem(Integer id, Resources resources, int drawableResourceId)
	{
		super(id);		
		this.file = null;
		this.drawableResourceID = drawableResourceId;
		TypedValue value = new TypedValue();
		resources.getValue(drawableResourceId, value, true);
		this.vectorBased = MediaHelpers.isVectorImageFileName(value.string.toString());
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
		try
		{
			if(!isVectorBased())
			{	// Raster image (PNG, JPG, GIF, ...):
				if(file != null)
					view.setImageBitmap(BitmapUtils.loadBitmap(context, file)); // use BitmapUtils for memory-safe load of (potentially very large) image
				else
					view.setImageResource(drawableResourceID);
			}
			else
			{	// Vector image (SVG or SVGZ):
				// 	Using svg-android lib:
				/*view.setImageDrawable(new SVGDrawable((file != null ?
					new SVGBuilder().readFromInputStream(new FileInputStream(file)) :
					new SVGBuilder().readFromResource(context.getResources(), drawableResourceID)).build()));*/
				// 	Using AndroidSVG lib:
				view.setImageDrawable(new PictureDrawable((file != null ?
					SVG.getFromInputStream(new FileInputStream(file)) :
					SVG.getFromResource(context.getResources(), drawableResourceID)).renderToPicture()));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG,	"Could not load image from " +
						(file != null ? "file" : "drawable resource") +
						"(" + (file != null ? file.getAbsolutePath() : "id: " + drawableResourceID) + ")", e);
		}
		
		return view;
	}

	public boolean isUsingFile()
	{
		return file != null;
	}

	public boolean isUsingResource()
	{
		return !isUsingFile();
	}
	
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

	/**
	 * @return the file
	 */
	public File getFile()
	{
		return file;
	}

	/**
	 * @return the drawableResourceID
	 */
	public int getDrawableResourceID()
	{
		return drawableResourceID;
	}

}
