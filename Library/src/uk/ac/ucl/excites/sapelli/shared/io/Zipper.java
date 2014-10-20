/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Helper class to create Zip files
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class Zipper
{
	private static final int BUFFER_SIZE = 2048;
	public static final String ZIP_EXTENTION = "zip";

	private ZipOutputStream zip;

	/**
	 * @param sourceFiles
	 * @param zipDestPath
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public void zip(List<File> sourceFiles, String zipDestPath) throws FileNotFoundException, IOException
	{
		// Make sure the zip files ends with the appropriate extension
		if(!FileHelpers.getFileExtension(zipDestPath).equalsIgnoreCase(ZIP_EXTENTION))
			zipDestPath += "." + ZIP_EXTENTION;
		
		zip(sourceFiles, new File(zipDestPath));
	}
	
	/**
	 * Zips the given list of files/folders to create the given destination zip archive file
	 * 
	 * @param sourceFiles
	 * @param zipDestination
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public void zip(List<File> sourceFiles, File zipDestination) throws FileNotFoundException, IOException
	{
		// Create containing folder:
		if(!zipDestination.exists())
			FileHelpers.createParentFolder(zipDestination);
	
		// Create the ZipOutputStream
		if(zip == null)
			zip = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipDestination)));
		
		// Iterate through all files and add them to the zip file
		for(File f : sourceFiles)
			if(f != null)
				zipFile(f);

		// Close the ZipOutputStream
		zip.close();
	}

	/**
	 * Zips a given file or folder
	 * 
	 * @param sourceFilePath
	 * @param zipDest
	 * @return whether successful
	 */
	private boolean zipFile(File sourceFile)
	{
		if(!sourceFile.exists())
			return false;
		// Try to zip it
		try
		{
			int basePathLength = sourceFile.getParentFile().getAbsolutePath().length() + 1; // +1 to include final slash
			// Check if file is a directory and use zipSubFolder()
			if(sourceFile.isDirectory())
				zipSubFolder(sourceFile, basePathLength);
			else
				zipFile(sourceFile, basePathLength);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	/**
	 * Zips a file
	 * 
	 * @param file
	 * @param basePathLength
	 * @throws IOException
	 */
	private void zipFile(File file, int basePathLength) throws IOException
	{
		BufferedInputStream source = null;
		try
		{
			byte[] data = new byte[BUFFER_SIZE];
			source = new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE);
			zip.putNextEntry(new ZipEntry(file.getAbsolutePath().substring(basePathLength))); // use path relative to basePath
			int count;
			while((count = source.read(data, 0, BUFFER_SIZE)) != -1)
				zip.write(data, 0, count);
		}
		finally
		{
			if(source != null)
				try
				{
					source.close();
				}
				catch(Exception ignore) {}
		}
	}

	/**
	 * Zips a (sub)folder
	 * 
	 * @param folder
	 * @param basePathLength
	 * @throws IOException
	 */
	private void zipSubFolder(File folder, int basePathLength) throws IOException
	{
		for(File file : folder.listFiles())
			if(file.isDirectory())
				zipSubFolder(file, basePathLength);
			else
				zipFile(file, basePathLength);
	}
	
}
