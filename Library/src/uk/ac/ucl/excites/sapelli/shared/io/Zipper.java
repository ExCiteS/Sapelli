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
 * Zipper class
 * 
 * @author Michalis Vitos
 *
 */
public class Zipper
{
	private static final int BUFFER_SIZE = 2048;
	public static final String ZIP_EXTENTION = "zip";

	private ZipOutputStream zip;
	private BufferedInputStream origin;
	private byte data[];

	private List<String> paths;
	private String zipDest;

	/**
	 * 
	 * @param paths
	 *            - A list of paths to files/folders to be zipped
	 * @param zipDest
	 *            - The destination of the zip file
	 */
	public Zipper(List<String> paths, String zipDest)
	{
		this.paths = paths;
		this.zipDest = zipDest;

		// Make sure that the parent folder of the zip exists
		final File zipFile = new File(zipDest);
		if(!zipFile.exists())
			FileHelpers.createParentFolder(zipFile);

		// Make sure the zip files ends with the appropriate extension
		if(!FileHelpers.getFileExtension(this.zipDest).equalsIgnoreCase(ZIP_EXTENTION))
			this.zipDest += "." + ZIP_EXTENTION;
	}

	/**
	 * Zips the list of files/folders
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public void zip() throws FileNotFoundException, IOException
	{
		// Create the ZipOutputStream
		if(zip == null)
			zip = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipDest)));

		// Iterate through all files and add them to the zip file
		for(String f : paths)
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
	 * @return
	 */
	private boolean zipFile(String sourceFilePath)
	{
		// Create and test the file
		File sourceFile = null;
			sourceFile = new File(sourceFilePath);
		
		if(!sourceFile.exists())
			return false;

		// Try to zip it
		try
		{
			// Check if file is a directory and use zipSubFolder()
			if(sourceFile.isDirectory())
			{
				zipSubFolder(sourceFile, sourceFile.getParent().length());
			}
			else
			{
				data = new byte[BUFFER_SIZE];
				FileInputStream fi = new FileInputStream(sourceFilePath);
				origin = new BufferedInputStream(fi, BUFFER_SIZE);
				ZipEntry entry = new ZipEntry(FileHelpers.getFileName(sourceFilePath));
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER_SIZE)) != -1)
				{
					zip.write(data, 0, count);
				}
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return false;
		}
		return true;
	}

	/**
	 * Zips a subfolder
	 * 
	 * @param folder
	 * @param basePathLength
	 * @throws IOException
	 */
	private void zipSubFolder(File folder, int basePathLength) throws IOException
	{
		File[] fileList = folder.listFiles();
		for(File file : fileList)
		{
			if(file.isDirectory())
			{
				zipSubFolder(file, basePathLength);
			}
			else
			{
				data = new byte[BUFFER_SIZE];
				String unmodifiedFilePath = file.getPath();
				String relativePath = unmodifiedFilePath.substring(basePathLength);

				FileInputStream fi = new FileInputStream(unmodifiedFilePath);
				origin = new BufferedInputStream(fi, BUFFER_SIZE);

				ZipEntry entry = new ZipEntry(relativePath);
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER_SIZE)) != -1)
				{
					zip.write(data, 0, count);
				}
			}
		}
	}
}
