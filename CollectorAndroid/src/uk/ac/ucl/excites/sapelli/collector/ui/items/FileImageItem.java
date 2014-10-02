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
import java.io.FileInputStream;

import uk.ac.ucl.excites.sapelli.collector.util.BitmapUtils;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.widget.ImageView;

//import com.caverock.androidsvg.SVG;
import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * An ImageItem subclass for images stored as files
 * 
 * @author mstevens
 */
public class FileImageItem extends ImageItem
{
	
	static private final String TAG = "FileImageItem";
	static private final int MAX_ITEM_WIDTH = 1200;

	private File file;
	
	public FileImageItem(File file)
	{
		this(null, file);
	}
	
	@SuppressLint("DefaultLocale")
	public FileImageItem(Integer id, File file)
	{
		super(id, FileHelpers.getFileExtension(file.getName()).toLowerCase().startsWith("svg")); //will also work for svgz files
		this.file = file;
	}
	
	@Override
	protected void setImage(Context context, ImageView view)
	{
		try
		{
			if(!isVectorBased())
			{
				// Raster image (PNG, JPG, GIF, ...):
				Bitmap imageBitmap = null;

				// Decode image size, do not create the actual bitmap (picture is null)
				BitmapFactory.Options options = new BitmapFactory.Options();
				options.inJustDecodeBounds = true;
				imageBitmap = BitmapFactory.decodeFile(file.getAbsolutePath(), options);

				// Find the preview size
				int previewWidth = (ScreenMetrics.GetScreenWidth(context) > 0) ? ScreenMetrics.GetScreenWidth(context) : MAX_ITEM_WIDTH;
				int previewHeight = (ScreenMetrics.GetScreenHeight(context) > 0) ? ScreenMetrics.GetScreenHeight(context) : MAX_ITEM_WIDTH;

				// Decode with inSampleSize and get the correct, scaled image
				options.inJustDecodeBounds = false;
				options.inSampleSize = BitmapUtils.calculateInSampleSize(options, previewWidth, previewHeight);
				imageBitmap = BitmapFactory.decodeFile(file.getAbsolutePath(), options);

				// Set the image
				view.setImageBitmap(imageBitmap);
			}
			else
			{	// Vector image (SVG or SVGZ):
				
				// Using svg-android lib:
				SVG svg = new SVGBuilder().readFromInputStream(new FileInputStream(file)).build();
				view.setImageDrawable(new SVGDrawable(svg));
				
				// Using AndroidSVG lib:
				//SVG svg = SVG.getFromInputStream(new FileInputStream(file));
		        //view.setImageDrawable(new PictureDrawable(svg.renderToPicture()));
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load image from file", e);
		}
	}
	
	public File getFile() {
		return file;
	}

}
