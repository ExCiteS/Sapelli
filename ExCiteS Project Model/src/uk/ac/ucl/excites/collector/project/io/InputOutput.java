package uk.ac.ucl.excites.collector.project.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.util.Log;

import com.google.common.io.Files;

public class InputOutput
{
	public static final String TAG = "InputOutput";

	/**
	 * Method to Copy a file
	 * 
	 * @param srcFilepath
	 * @param dstFilepath
	 * @throws IOException
	 */
	public static void copyFile(String srcFilepath, String dstFilepath)
	{
		try
		{
			// Create the files
			File srcFile = new File(srcFilepath);
			File dstFile = new File(dstFilepath);

			// Get the parent directory
			File parentDir = new File(dstFile.getParentFile().getAbsolutePath());
			parentDir.mkdirs();

			if(!dstFile.exists())
			{
				dstFile.createNewFile();
			}

			InputStream in = new FileInputStream(srcFile);
			OutputStream out = new FileOutputStream(dstFile);

			// Transfer bytes from in to out
			byte[] buf = new byte[1024];
			int len;
			while((len = in.read(buf)) > 0)
			{
				out.write(buf, 0, len);
			}
			in.close();
			out.close();
		}
		catch(IOException e)
		{
			Log.e(TAG, "Error: ", e);
		}
	}

	/**
	 * Delete a file
	 * @param deleteFile
	 */
	public static void deleteFile(String deleteFile)
	{
		new File(deleteFile).delete();
	}

	/**
	 * Move a file
	 * @param srcFilepath
	 * @param dstFilepath
	 */
	public static void moveFile(String srcFilepath, String dstFilepath)
	{
		try
		{
			Files.move(new File(srcFilepath), new File(dstFilepath));
		}
		catch(IOException e)
		{
			Log.e(TAG, "Error: ", e);
		}
	}

}
