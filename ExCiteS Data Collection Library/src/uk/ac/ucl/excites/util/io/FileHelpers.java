package uk.ac.ucl.excites.util.io;

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
	{	//TODO fix this properly
		return true; //Unix only: return path.charAt(0) == '/';
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
	 * Copies a file. If the destination exists it is overwritten.
	 * 
	 * @param srcFilePath
	 * @param dstFilePath
	 * @throws IOException
	 */
	public static void copyFile(String srcFilePath, String dstFilePath) throws IOException
	{
		copyFile(new File(srcFilePath), new File(dstFilePath));
	}

	/**
	 * Copies a file. If the destination exists it is overwritten.
	 * 
	 * @param srcFile
	 * @param dstFile
	 * @throws IOException
	 */
	public static void copyFile(File srcFile, File dstFile) throws IOException
	{
		InputStream in = null;
		OutputStream out = null;
		try
		{
			in = new FileInputStream(srcFile);
			// Get the parent directory
			File parentDir = new File(dstFile.getParentFile().getAbsolutePath());
			parentDir.mkdirs();

			// Create file if it doesn't exist (if it exists it will be overwritten)
			if(!dstFile.exists())
				dstFile.createNewFile();

			out = new FileOutputStream(dstFile);

			// Transfer bytes from in to out
			byte[] buf = new byte[1024];
			int len;
			while((len = in.read(buf)) > 0)
				out.write(buf, 0, len);
			in.close();
			out.close();
		}
		catch(Exception e)
		{
			try
			{
				if(in != null)
					in.close();
				if(out != null)
					out.close();
			}
			catch(Exception ignore) {}
			throw new IOException("Error on copying file", e);
		}
	}

	/**
	 * Move a file. If the destination exists it is overwritten.
	 * 
	 * @param srcFilepath
	 * @param dstFilepath
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveFile(String srcFilepath, String dstFilepath) throws IOException, IllegalArgumentException
	{
		moveFile(new File(srcFilepath), new File(dstFilepath));
	}
	
	/**
	 * Move a file. If the destination exists it is overwritten.
	 * 
	 * @param srcFile
	 * @param dstFile
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveFile(File srcFile, File dstFile) throws IOException, IllegalArgumentException
	{
		if(srcFile.equals(dstFile))
			throw new IllegalArgumentException("Source and destination files must be different.");
		if(!srcFile.renameTo(dstFile))
		{
			copyFile(srcFile, dstFile);
			if(!srcFile.delete())
				throw new IOException("Unable to delete " + srcFile.getAbsolutePath());
		}
	}
	
	/**
	 * Moves a directory. Files that already exist in the destination directory are overwritten.
	 * 
	 * @param srcPath
	 * @param dstPath
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveDirectory(String srcPath, String dstPath) throws IOException, IllegalArgumentException
	{
		moveDirectory(new File(srcPath), new File(dstPath));
	}
	
	/**
	 * Moves a directory. Files that already exist in the destination directory are overwritten. Files that exist in the distination directory but not in the source are left alone.
	 * 
	 * @param srcFolder
	 * @param dstFolder
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveDirectory(File srcFolder, File dstFolder) throws IOException, IllegalArgumentException
	{
		if(!srcFolder.exists())
			throw new IllegalArgumentException("Source directory does not exist");
		if(!srcFolder.isDirectory())
			throw new IllegalArgumentException("Source is not a directory, call moveFile() instead");
		if(!dstFolder.isDirectory())
			throw new IllegalArgumentException("Destination is not a directory, call moveFile() instead");
		
		// Create destination if needed:
		createFolder(dstFolder);
		
		// Move contents (recursive calls will happen for subdirectories):
		for(File source : srcFolder.listFiles())
			moveFileOrDirectory(source, new File(dstFolder.getAbsolutePath() + File.separator + source.getName()));
		
		// Check if srcFolder is empty:
		if(!isFolderEmpty(srcFolder))
			throw new IOException("Some contents may not have been moved or copied.");
		
		// Delete srcFolder:
		if(!srcFolder.delete())
			throw new IOException("Could not delete srcFolder, ");
	}
	
	/**
	 * Move a file or directory. Existing files are overwritten.
	 * 
	 * @param srcPath
	 * @param dstPath
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveFileOrDirectory(String srcPath, String dstPath) throws IOException, IllegalArgumentException
	{
		moveFileOrDirectory(new File(srcPath), new File(dstPath));
	}
	
	/**
	 * Move a file or directory
	 * 
	 * @param source
	 * @param destination
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveFileOrDirectory(File source, File destination) throws IOException, IllegalArgumentException
	{
		if(source.isDirectory())
			moveDirectory(source, destination);
		else
			moveFile(source, destination);
	}
	
	public static boolean isFolderEmpty(File directory)
	{
		if(!directory.isDirectory())
			throw new IllegalArgumentException("File is not a directory");
		File[] contents = directory.listFiles();
		return contents == null || contents.length == 0;
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
	 * @return The extension of the filePath. If there is no extension the empty String is returned.
	 */
	static public String getFileExtension(String filePath)
	{
		int lastIndex = filePath.lastIndexOf(".");
		if(lastIndex == -1)
			return "";
		return filePath.substring(lastIndex + 1, filePath.length());
	}

	/**
	 * Returns the filePath without its extension. e.g for "/my/path/myfile.raw" : "/my/path/myfile"
	 * 
	 * @param filePath
	 * @return The filePath without its extension. If these is not extension the path is return as-is.
	 */
	static public String trimFileExtensionAndDot(String filePath)
	{
		int lastIndex = filePath.lastIndexOf(".");
		if(lastIndex == -1)
			return filePath;
		return filePath.substring(0, lastIndex - 1);
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
