package uk.ac.ucl.excites.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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

	// Strategies for opening FileConnection on an existing file:
	static final public int FILE_EXISTS_STRATEGY_REPLACE = 0;
	static final public int FILE_EXISTS_STRATEGY_REJECT = 1;
	static final public int FILE_EXISTS_STRATEGY_CREATE_RENAMED_FILE = 2;
	static final public int FILE_EXISTS_STRATEGY_RENAME_EXISTING_FILE = 3;
	static final public int FILE_EXISTS_STRATEGY_APPEND = 4;

	// Strategies for opening FileConnection on a non-existing file:
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

	static public boolean isValidPath(String path)
	{
		return path.charAt(0) == '/';
	}

	static public boolean isFolderPath(String fullPath)
	{
		return isValidPath(fullPath) && fullPath.charAt(fullPath.length() - 1) == File.separatorChar;
	}
	
	/**
	 * Ensures that the path is a folder path (adding / or \ if needed, but does not check if the folder exists)
	 * 
	 * @param path
	 * @return
	 */
	static public String ensureFolderPath(String path)
	{
		return path + (path.charAt(path.length() - 1) == File.separatorChar ? "" : File.separatorChar); 
	}

	static public boolean isFilePath(String fullPath)
	{
		return isValidPath(fullPath) && !isFolderPath(fullPath);
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
		InputStream inputStream;
		try
		{
			inputStream = new FileInputStream(srcFile);
			copyFile(inputStream, dstFile);
		}
		catch(FileNotFoundException e)
		{
			System.err.println("FileIO error: " + e.getLocalizedMessage());
			e.printStackTrace();
		}
	}

	public static void copyFile(InputStream input, File dstFile)
	{
		try
		{
			// Get the parent directory
			File parentDir = new File(dstFile.getParentFile().getAbsolutePath());
			parentDir.mkdirs();

			if(!dstFile.exists())
				dstFile.createNewFile();

			OutputStream out = new FileOutputStream(dstFile);

			// Transfer bytes from in to out
			byte[] buf = new byte[1024];
			int len;
			while((len = input.read(buf)) > 0)
			{
				out.write(buf, 0, len);
			}
			input.close();
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
	 * @return success (whether the directory exists now, or existed already)
	 */
	public static boolean createFolder(File folder)
	{
		if(!folder.exists() || !folder.isDirectory())
			return folder.mkdirs();
		return true;
	}

	/**
	 * This function returns a folder, defined by the folderPath String (e.g. "/my/data/folder/") It will not create it if it doesn't already exists.
	 * 
	 * @param folderPath
	 * @return
	 */
	static public File getFolder(String folderPath)
	{
		if(!isFolderPath(folderPath))
			return null;
		File folder = new File(folderPath);
		return folder;
	}

	/**
	 * Returns only the folderPath of the filePath. e.g. for "/my/path/myfile.raw" : "/my/path/"
	 * 
	 * @param filePath
	 * @return The folderPath of the filePath. e.g. for "/my/path/myfile.raw" : "/my/path/"
	 */
	static public String getFolderPath(String filePath)
	{
		int lastIndex = filePath.lastIndexOf("/");
		String result = filePath.substring(0, lastIndex + 1);
		return result;
	}
	
	/**
	 * @param file
	 * @return the extension of a file represented by a File object, returns the separator char (/ or \) in case of a directory
	 */
	static public String getFileExtension(File file)
	{
		if(file.isFile())
			return getFileExtension(file.getAbsolutePath());
		else
			return File.separator;
	}

	/**
	 * Returns the extension of the filePath. e.g. for "/my/path/myfile.raw" : "raw"
	 * 
	 * @param filePath
	 * @return The extension of the filePath. e.g. for "/my/path/myfile.raw" : "raw"
	 */
	static public String getFileExtension(String filePath)
	{
		int lastIndex = filePath.lastIndexOf(".");
		String result = filePath.substring(lastIndex + 1, filePath.length());
		return result;
	}

	/**
	 * Returns the filePath without its extension. e.g for "/my/path/myfile.raw" : "/my/path/myfile"
	 * 
	 * @param filePath
	 * @return The filePath without its extension. e.g for "/my/path/myfile.raw" : "/my/path/myfile"
	 */
	static public String trimFileExtensionAndDot(String filePath)
	{
		int lastIndex = filePath.lastIndexOf(".");
		String result = filePath.substring(0, lastIndex - 1);
		return result;
	}

	/**
	 * Returns only the fileName of the filePath. e.g. for "/my/path/myfile.raw" : "myFile.raw"
	 * 
	 * @param filePath
	 * @return The fileName of the filePath. e.g. for "/my/path/myfile.raw" : "myFile.raw"
	 */
	static public String getFileName(String filePath)
	{
		int lastIndex = filePath.lastIndexOf("/");
		String result = filePath.substring(lastIndex + 1, filePath.length());
		return result;
	}
	
	/**
	 * @param file
	 * @return true if the file object is not null and represents an existing, readable file, false otherwise
	 */
	static public boolean isReadableFile(File file)
	{
		return file != null && file.exists() && file.isFile() && file.canRead();
	}

	/**
	 * @param file
	 * @return true if the file object is not null and represents an existing, read/writable directory, false otherwise
	 */
	static public boolean isReadableWritableDirectory(File directory)
	{
		return directory != null && directory.exists() && directory.isDirectory() && directory.canRead() && directory.canWrite();
	}
	
}
