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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.BitmapFactory.Options;

/**
 * Helper methods to deal with (potentially very large) raster images (i.e. bitmaps).
 * Based on examples from: http://developer.android.com/training/displaying-bitmaps/load-bitmap.html
 * 
 * @author Michalis Vitos, mstevens
 */
public class BitmapUtils
{
	
	static private final int DEFAULT_REQ_WIDTH = 1200;
	static private final int DEFAULT_REQ_HEIGHT = 1200;
	
	private BitmapUtils() {}

	public static Bitmap loadBitmap(Context context, final File imageFile)
	{
		return loadBitmap(context, new DecodeDelegate()
		{
			@Override
			public Bitmap decode(Options options)
			{
				return BitmapFactory.decodeFile(imageFile.getAbsolutePath(), options);
			}
		});
	}
	
	public static Bitmap loadBitmap(final Context context, final int resourceId)
	{
		return loadBitmap(context, new DecodeDelegate()
		{
			@Override
			public Bitmap decode(Options options)
			{
				return BitmapFactory.decodeResource(context.getResources(), resourceId);
			}
		});
	}
	
	public static Bitmap loadBitmap(final Context context, final byte[] bytes)
	{
		return loadBitmap(context, new DecodeDelegate()
		{
			@Override
			public Bitmap decode(Options options)
			{
				return BitmapFactory.decodeByteArray(bytes, 0, bytes.length, options);
			}
		});
	}
	
	private static interface DecodeDelegate
	{
	
		public Bitmap decode(BitmapFactory.Options options);
		
	}
	
	private static Bitmap loadBitmap(Context context, DecodeDelegate delegate)
	{
		// Decode image size, do not create the actual bitmap
		BitmapFactory.Options options = new BitmapFactory.Options();
		options.inJustDecodeBounds = true;
		delegate.decode(options); // sets options.outWidth & options.outHeight

		// Find the preview size
		int previewWidth = (ScreenMetrics.GetScreenWidth(context) > 0) ? ScreenMetrics.GetScreenWidth(context) : DEFAULT_REQ_WIDTH;
		int previewHeight = (ScreenMetrics.GetScreenHeight(context) > 0) ? ScreenMetrics.GetScreenHeight(context) : DEFAULT_REQ_HEIGHT;

		// Decode with inSampleSize and get the correct, scaled image
		options.inJustDecodeBounds = false; // full decode
		options.inSampleSize = BitmapUtils.calculateInSampleSize(options, previewWidth, previewHeight);
		return delegate.decode(options);
	}
	
	/**
	 * @param options
	 * @param reqWidth
	 * @param reqHeight
	 * @return
	 */
	private static int calculateInSampleSize(BitmapFactory.Options options, int reqWidth, int reqHeight)
	{
		// Raw height and width of image
		final int height = options.outHeight;
		final int width = options.outWidth;
		int inSampleSize = 1;

		if(height > reqHeight || width > reqWidth)
		{
			// Calculate the largest inSampleSize value that is a power of 2 and keeps both
			// height and width larger than the requested height and width.
			while((height / inSampleSize) > reqHeight && (width / inSampleSize) > reqWidth)
				inSampleSize *= 2;
		}
		return inSampleSize;
	}
	
}
