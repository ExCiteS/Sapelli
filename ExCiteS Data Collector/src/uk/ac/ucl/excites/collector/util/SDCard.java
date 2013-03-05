package uk.ac.ucl.excites.collector.util;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Environment;

/**
 * SD Card utility class
 * 
 * @author Michalis Vitos
 */
public class SDCard
{

	public static final String TAG = "FileHelpers";

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

	public static void showError(final Activity activity)
	{
		new AlertDialog.Builder(activity).setTitle("SD Card Error").setMessage("ExCiteS needs an SD Card in order to function. Please insert one and launch again the application.").setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				activity.finish();
			}
		}).create().show();
	}
}
