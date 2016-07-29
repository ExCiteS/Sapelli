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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author mstevens
 *
 */
public final class Unzipper
{

	private Unzipper() {}
	
	/**
	 * Note that if the zipFileStream is not a ZIP file there will be no exception thrown,
	 * but also no files extracted (i.e. method returns 0).
	 * 
	 * @param zipFileStream
	 * @param extractionFolder
	 * @return the number of extracted entries
	 * @throws IOException - always wraps around a causing Exception
	 */
	static public int unzip(InputStream zipFileStream, File extractionFolder) throws IOException
	{
		try
		{
			String extractionPath = extractionFolder.getAbsolutePath() + File.separator;
			ZipInputStream zin = new ZipInputStream(zipFileStream);
			int entryCount = 0;
			ZipEntry ze = null;
			while((ze = zin.getNextEntry()) != null)
			{
				entryCount++;
				if(ze.isDirectory())
				{
					if(!FileHelpers.createDirectory(extractionPath + ze.getName()))
					{
						zin.close();
						throw new IOException("Could not create folder: " + extractionPath + ze.getName());
					}
				}
				else
				{
					FileOutputStream fout = new FileOutputStream(extractionPath + ze.getName(), false);
					byte[] buffer = new byte[4096];
					for(int c = zin.read(buffer); c != -1; c = zin.read(buffer))
						fout.write(buffer, 0, c);
					fout.close();
				}
				zin.closeEntry();
			}
			zin.close();
			return entryCount;
		}
		catch(Exception e)
		{
			throw new IOException("Error on unzipping archive", e);
		}
	}
	
	public static InputStream getInputStreamForFileInZip(InputStream zipFileStream, String filename) throws IOException
	{
		ZipInputStream zin = new ZipInputStream(zipFileStream);
		ZipEntry ze = null;
		while((ze = zin.getNextEntry()) != null)
		{
			if(ze.getName().equalsIgnoreCase(filename))
				return zin; // stream is now positioned to read the indicated file
		}
		throw new IOException(filename + " not found in archive.");
	}

}
