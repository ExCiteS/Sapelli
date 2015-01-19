package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.IOException;

import android.util.Log;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;

/**
 * Android-specific subclass of Logger which mirrors log writes to Android Logcat while writing to file.
 * 
 * @author benelliott
 */
public class AndroidLogger extends Logger
{

	private static String TAG = "Sapelli logger";
	
	public AndroidLogger(String folderPath, String baseFileName, boolean printToOutputStream) throws IOException
	{
		super(folderPath, baseFileName, printToOutputStream);
	}
	
	@Override
	protected void printToOutputStream(String line)
	{
		if (printToOutputStream)
			Log.i(TAG, line);
	}
	
	@Override
	protected void printToOutputStream(String time, String... fields)
	{
		if (printToOutputStream)
		{
			StringBuilder stringBuilder = new StringBuilder(time);
			for (String field : fields)
				stringBuilder.append(FIELD_SEPARATOR + field);
			stringBuilder.append(FIELD_SEPARATOR);
			
			Log.i(TAG, stringBuilder.toString());
		}
	}

}
