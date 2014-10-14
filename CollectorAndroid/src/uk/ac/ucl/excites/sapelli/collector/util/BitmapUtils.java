package uk.ac.ucl.excites.sapelli.collector.util;

import android.graphics.BitmapFactory;

public class BitmapUtils
{
	BitmapUtils() {}

	/**
	 * http://developer.android.com/training/displaying-bitmaps/load-bitmap.html
	 * 
	 * @param options
	 * @param reqWidth
	 * @param reqHeight
	 * @return
	 */
	public static int calculateInSampleSize(BitmapFactory.Options options, int reqWidth, int reqHeight)
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
