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
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Helper class to create Zip files
 * 
 * @author Michalis Vitos, mstevens
 */
public final class Zipper
{
	private static final int BUFFER_SIZE = 2048;
	public static final String ZIP_EXTENSION = "zip";

	private Zipper()
	{
		// this class should never be instantiated
	}
	
	/**
	 * Zips the given list of files/folders to create the a zip archive file at the given path
	 * 
	 * @param zipDestinationPath
	 * @param sourceFiles
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	static public void Zip(String zipDestinationPath, File... sourceFiles) throws FileNotFoundException, IOException
	{
		// Make sure the zip file ends with the appropriate extension
		if(!FileHelpers.getFileExtension(zipDestinationPath).equalsIgnoreCase(ZIP_EXTENSION))
			zipDestinationPath += "." + ZIP_EXTENSION;
		
		Zip(new File(zipDestinationPath), sourceFiles);
	}
	
	/**
	 * Zips the given list of files/folders to create the given destination zip archive file
	 * 
	 * @param zipDestination the destination file, if it already exists it will be overwritten
	 * @param sourceFiles
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	static public void Zip(File zipDestination, File... sourceFiles) throws FileNotFoundException, IOException
	{
		// Create containing folder:
		if(!zipDestination.exists())
			FileHelpers.createParentFolder(zipDestination);
	
		ZipOutputStream zipOutputStream = null;
		try
		{
			// Create the ZipOutputStream
			zipOutputStream = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipDestination, false)));
		
			// Loop through all source files/folder and add them to the zip file
			if(sourceFiles != null)
				for(File sourceFile : sourceFiles)
					if(sourceFile != null && sourceFile.exists())
					{
						int basePathLength = sourceFile.getParentFile().getAbsolutePath().length() + 1; // +1 to include final slash
						if(sourceFile.isDirectory())
							zipFolder(zipOutputStream, sourceFile, basePathLength);
						else
							zipFile(zipOutputStream, sourceFile, basePathLength);
					}
		}
		finally
		{
			if(zipOutputStream != null)
				try
				{	// Close the ZipOutputStream
					zipOutputStream.close();
				}
				catch(Exception ignore) {}
		}
	}
	
	/**
	 * Zips a file
	 * 
	 * @param zipOutputStream
	 * @param file
	 * @param basePathLength
	 * @throws IOException
	 */
	static private void zipFile(ZipOutputStream zipOutputStream, File file, int basePathLength) throws IOException
	{
		BufferedInputStream source = null;
		try
		{
			byte[] data = new byte[BUFFER_SIZE];
			source = new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE);
			zipOutputStream.putNextEntry(new ZipEntry(file.getAbsolutePath().substring(basePathLength))); // use path relative to basePath
			int count;
			while((count = source.read(data, 0, BUFFER_SIZE)) != -1)
				zipOutputStream.write(data, 0, count);
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
	 * @param zipOutputStream
	 * @param folder
	 * @param basePathLength
	 * @throws IOException
	 */
	private static void zipFolder(ZipOutputStream zipOutputStream, File folder, int basePathLength) throws IOException
	{
		for(File file : folder.listFiles())
			if(file.isDirectory())
				zipFolder(zipOutputStream, file, basePathLength);
			else
				zipFile(zipOutputStream, file, basePathLength);
	}
	
}
