package uk.ac.ucl.excites.collector.project.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


/**
 * File I/O helpers
 * 
 * @author mstevens, Michalis Vitos
 * 
 */
public final class FileHelpers
{
	
	//Strategies for opening FileConnection on an existing file:
	static final public int FILE_EXISTS_STRATEGY_REPLACE = 0;
	static final public int FILE_EXISTS_STRATEGY_REJECT = 1;
	static final public int FILE_EXISTS_STRATEGY_CREATE_RENAMED_FILE = 2;
	static final public int FILE_EXISTS_STRATEGY_RENAME_EXISTING_FILE = 3;
	static final public int FILE_EXISTS_STRATEGY_APPEND = 4;
	
	//Strategies for opening FileConnection on a non-existing file:
	static final public int FILE_DOES_NOT_EXIST_STRATEGY_REJECT = 1;
	static final public int FILE_DOES_NOT_EXIST_STRATEGY_CREATE = 2;

	private FileHelpers()
	{
	} // no-one should instantiate this class

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
		copyFile(new File(srcFilepath), new File(dstFilepath));
	}
	
	public static void copyFile(File srcFile, File dstFile)
	{
		try
		{
			// Get the parent directory
			File parentDir = new File(dstFile.getParentFile().getAbsolutePath());
			parentDir.mkdirs();

			if(!dstFile.exists())
				dstFile.createNewFile();
			
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
	 * 
	 * @param filePath
	 * @return whether the file was deleted or not
	 */
	public static boolean deleteFile(String filePath)
	{
		return (new File(filePath)).delete();
	}

	/**
	 * Move a file
	 * 
	 * @param srcFilepath
	 * @param dstFilepath
	 */
	public static void moveFile(String srcFilepath, String dstFilepath)
	{
		try
		{
			File from = new File(srcFilepath);
			File to = new File(dstFilepath);
			if(!from.equals(to))
				throw new IllegalArgumentException("Source and destination files must be different.");
			if(!from.renameTo(to))
			{
			      copyFile(from, to);
			      if(!from.delete())
			    	  throw new IOException("Unable to delete " + from);
			}
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
		return createFolder(new File(folderPath));
	}
	
	/**
	 * Attempts to create the necessary (containing) folder(s) for a given path
	 * 
	 * @param folderPath
	 * @return success (whether the directory exists now)
	 */
	public static boolean createFolder(File folder)
	{
		if(!folder.exists() || !folder.isDirectory())
			return folder.mkdirs();
		return true;
	}

}
