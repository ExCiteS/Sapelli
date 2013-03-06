package uk.ac.ucl.excites.collector.util;

import android.os.Environment;

/**
 * SD Card utility class
 * 
 * @author Michalis Vitos
 */
public class SDCard
{

	public static final String TAG = "SDCard";

	private SDCard()
	{
	} // no-one should instantiate this class

	/**
	 * Checks if external storage is available for read and write
	 * 
	 * @return
	 */
	public static boolean isExternalStorageWritable()
	{
		String state = Environment.getExternalStorageState();
		if(Environment.MEDIA_MOUNTED.equals(state))
		{
			return true;
		}
		return false;
	}

	/**
	 * Checks if external storage is available to at least read
	 * 
	 * @return
	 */
	public static boolean isExternalStorageReadable()
	{
		String state = Environment.getExternalStorageState();
		if(Environment.MEDIA_MOUNTED.equals(state) || Environment.MEDIA_MOUNTED_READ_ONLY.equals(state))
		{
			return true;
		}
		return false;
	}
	
}
