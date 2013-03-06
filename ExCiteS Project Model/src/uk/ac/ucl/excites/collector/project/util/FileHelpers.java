package uk.ac.ucl.excites.collector.project.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.google.common.io.Files;

/**
 * File I/O helpers
 * 
 * @author mstevens, Michalis Vitos
 *
 */
public final class FileHelpers
{
	public static final String TAG = "FileHelpers";
	
	private FileHelpers() { } //no-one should instantiate this class

	static public boolean isValidFileName(String filename)
	{
		if(filename.contains("*"))
			return false;
		if(filename.contains("?"))
			return false;
		if(filename.contains("<"))
			return false;
		if(filename.contains(">"))
			return false;
		if(filename.contains(":"))
			return false;
		if(filename.contains("\""))
			return false;
		if(filename.contains("\\"))
			return false;
		if(filename.contains("/"))
			return false;
		if(filename.contains("|"))
			return false;
		if(filename.contains("\n"))
			return false;
		if(filename.contains("\t"))
			return false;
		return true;
	}

	static public String makeValidFileName(String filename)
	{
		if(filename != null)
		{
			filename = filename.replace('*', '+');
			filename = filename.replace('?', '_');
			filename = filename.replace('<', '(');
			filename = filename.replace('>', ')');
			filename = filename.replace(':', '-');
			filename = filename.replace('"', '\'');
			filename = filename.replace('\\', '_');
			filename = filename.replace('/', '_');
			filename = filename.replace('|', ';');
			filename = filename.replace('\n', '_');
			filename = filename.replace('\t', '_');
		}
		return filename;
	}
	
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
			System.err.println("FileIO error: " + e.getLocalizedMessage());
			e.printStackTrace();
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
			System.err.println("FileIO error: " + e.getLocalizedMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Attempts to create the necessary (containing) folder(s) for a given path 
	 * 
	 * @param folderPath
	 * @return success (whether the directory exists now)
	 */
	public static boolean createFolder(String folderPath)
	{
		File folder = new File(folderPath);
		if(!folder.exists() || !folder.isDirectory())
			return folder.mkdirs();
		return true;
	}
}
