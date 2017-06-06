/**
 * Sapelli data collection platform: http://sapelli.org
 *
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.shared.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;

import org.apache.commons.io.FileUtils;

/**
 * File I/O helpers
 *
 * @author mstevens, Michalis Vitos
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

	/**
	 * This class should never be instantiated 
	 */
	private FileHelpers()
	{
	}

	static public boolean isValidFileName(String filename)
	{
		if(filename == null)
			return false;
		for(char c : filename.toCharArray())
		{
			switch(c)
			{
				case '*':
				case '?':
				case '<':
				case '>':
				case ':':
				case '"':
				case '\\':
				case '/':
				case '|':
				case '\n':
				case '\r':
				case '\t': return false;
			}
		}
		return true;
	}

	/**
	 * Ensures that the path is a directory path (adding / or \ if needed, but does not check if the directory actually exists)
	 *
	 * @param path
	 * @return
	 */
	static public String ensureDirectoryPath(String path)
	{
		return path + (path.charAt(path.length() - 1) == File.separatorChar ? "" : File.separatorChar);
	}

	static public String makeValidFileName(String filename)
	{
		if(filename == null)
			return null;
		char[] chars = filename.toCharArray();
		for(int c = 0; c < chars.length; c++)
		{
			switch(chars[c])
			{
				case '*': chars[c] = '+'; break;
				case '?': chars[c] = '_'; break;
				case '<': chars[c] = '('; break;
				case '>': chars[c] = ')'; break;
				case ':': chars[c] = '-'; break;
				case '"': chars[c] = '\''; break;
				case '\\': chars[c] = '_'; break;
				case '/': chars[c] = '_'; break;
				case '|': chars[c] = ';'; break;
				case '\n': chars[c] = '_'; break;
				case '\r': chars[c] = '_'; break;
				case '\t': chars[c] = '_'; break;
			}
		}
		return new String(chars);
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
	 * Quietly deletes a bunch of {@link File}s. 
	 *
	 * @param files list of files to quietly delete (may be {@code null})
	 *
	 * @see FileUtils#deleteQuietly(File)
	 */
	public static void deleteQuietly(Collection<File> files)
	{
		if(files != null)
			for(File file : files)
				FileUtils.deleteQuietly(file);
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
			StreamHelpers.SilentClose(in);
			StreamHelpers.SilentClose(out);
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
	 * Moves a directory. Files that already exist in the destination directory are overwritten. Files that exist in the destination directory but not in the source are left alone.
	 *
	 * @param srcDir
	 * @param dstDir
	 * @throws IOException
	 * @throws IllegalArgumentException
	 */
	public static void moveDirectory(File srcDir, File dstDir) throws IOException, IllegalArgumentException
	{
		if(!srcDir.exists())
			throw new IllegalArgumentException("Source directory does not exist");
		if( !srcDir.isDirectory())
			throw new IllegalArgumentException("Source is not a directory, call moveFile() instead");

		// Create destination if needed:
		if(!dstDir.exists())
			createDirectory(dstDir);
		else if(!dstDir.isDirectory())
			throw new IllegalArgumentException("Destination exists but is not a directory!");

		// Move contents (recursive calls will happen for subdirectories):
		for(File source : srcDir.listFiles())
			moveFileOrDirectory(source, new File(dstDir.getAbsolutePath() + File.separator + source.getName()));

		// Check if srcDir is empty:
		if(!isDirectoryEmpty(srcDir))
			throw new IOException("Some contents may not have been moved or copied.");

		// Delete srcDir:
		if(!srcDir.delete())
			throw new IOException("Could not delete source directory (" + srcDir.getAbsolutePath() + ").");
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

	public static void deleteDirectoryIfEmpty(File directory)
	{
		try
		{
			if(isDirectoryEmpty(directory))
				FileUtils.deleteQuietly(directory);
		}
		catch(Exception ignore) {}
	}

	public static boolean isDirectoryEmpty(File directory)
	{
		if(!directory.isDirectory())
			throw new IllegalArgumentException("File is not a directory");
		File[] contents = directory.listFiles();
		return contents == null || contents.length == 0;
	}

	/**
	 * Attempts to create the necessary (containing) directory/ies for a given path
	 *
	 * @param directoryPath
	 * @return success (whether the directory exists now, or already existed)
	 */
	public static boolean createDirectory(String directoryPath)
	{
		return createDirectory(new File(directoryPath));
	}

	/**
	 * Attempts to create the necessary (containing) directory/ies for a given path
	 *
	 * @param directory
	 * @return success, i.e. whether the directory exists now (as a directory, *not* as a file), or existed already
	 */
	public static boolean createDirectory(File directory)
	{
		if(directory == null)
			return false;
		try
		{
			if(!directory.exists())
				return directory.mkdirs();
			return directory.isDirectory();
		}
		catch(SecurityException se)
		{
			se.printStackTrace(System.err);
			return false;
		}
	}

	/**
	 * Returns (as a File instance) a subdirectory with the given name in the given parent directory.
	 * If {@code create} is {@code true} the directory is created on disc (if the parent directory does not exist it is created as well).
	 *
	 * @param parentDir
	 * @param subDirName
	 * @param create
	 * @return
	 * @throws IOException
	 */
	public static File getSubDirectory(File parentDir, String subDirName, boolean create) throws IOException
	{
		File subDir = new File(parentDir, subDirName);
		if(create)
		{	// Create and test the sub dir
			if(!createDirectory(subDir))
				throw new IOException("Could not create directory: " + subDir.getAbsolutePath());
		}
		return subDir;
	}

	/**
	 * Attempts to create the necessary parent directory/ies for a given path (the path could be a directory or file)
	 *
	 * @param path
	 * @return
	 */
	public static boolean createParentDirectory(File path)
	{
		return createDirectory(path.getParentFile());
	}

	/**
	 * Returns only the parent directory path of the given file path,
	 * e.g. for "/my/path/myfile.raw" : "/my/path/"
	 *
	 * @param filePath
	 * @return The parent path of the filePath
	 */
	static public String getParentPath(String filePath)
	{
		return filePath.substring(0, filePath.lastIndexOf(File.separatorChar) + 1);
	}

	/**
	 * @param file
	 * @return the extension of a file represented by a File object, returns the separator char (/ or \) in case of a directory
	 */
	static public String getFileExtension(File file)
	{
		if(file.isFile())
			return getFileExtension(file.getName());
		else
			return "" + File.separatorChar;
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
		return filePath.substring(0, lastIndex);
	}

	/**
	 * Returns only the fileName of the filePath. e.g. for "/my/path/myfile.raw" : "myFile.raw"
	 *
	 * @param filePath
	 * @return The fileName of the filePath. e.g. for "/my/path/myfile.raw" : "myFile.raw"
	 */
	static public String getFileName(String filePath)
	{
		int lastIndex = filePath.lastIndexOf(File.separator);
		String result = filePath.substring(lastIndex + 1, filePath.length());
		return result;
	}

	/**
	 * @param file
	 * @return true if the file object is not null and represents an existing, readable file, false otherwise
	 */
	static public boolean isReadableFile(File file)
	{
		try
		{
			return file != null && file.exists() && file.isFile() && file.canRead();
		}
		catch(SecurityException se)
		{
			se.printStackTrace(System.err);
			return false;
		}
	}

	/**
	 * @param file
	 * @return true if the file object is not null and represents an existing, read/writable directory, false otherwise
	 */
	static public boolean isReadableWritableDirectory(File directory)
	{
		try
		{
			return directory != null && directory.exists() && directory.isDirectory() && directory.canRead() && directory.canWrite();
		}
		catch(SecurityException se)
		{
			se.printStackTrace(System.err);
			return false;
		}
	}

	/**
	 * @param file
	 * @param refuseEmpty whether or not to refuse empty files (i.e. size = 0 bytes)
	 * @return a FileInputStream
	 * @throws IllegalArgumentException when the file is empty and refuseEmpty is true
	 * @throws FileNotFoundException when the file does not exist
	 * @throws SecurityException when we are not allowed to read the file 
	 * @throws NullPointerException when file is null
	 */
	static public FileInputStream openInputStream(File file, boolean refuseEmpty) throws IllegalArgumentException, FileNotFoundException, SecurityException, NullPointerException
	{
		if(file != null && file.exists() && refuseEmpty && file.length() == 0)
			throw new IllegalArgumentException("File \"" + file.getAbsolutePath() + "\" is empty!");
		return new FileInputStream(file);
	}

	/**
	 * Get the relative path of a file. For example for "C:\Users\Desktop\Projects\GPS Test 5m\v3.0\img\Back.png" get the "\img\Back.png"
	 *
	 * @param sourceDir the source directory
	 * @param file the file
	 * @return relative path of the file
	 */
	public static String getRelativePath(File sourceDir, File file)
	{
		// Trim off the start of source dir path...
		String path = file.getPath().substring(sourceDir.toString().length());
		if(path.startsWith(File.pathSeparator))
		{
			path = path.substring(1);
		}
		return path;
	}
}
