/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.util.io;

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
	
	static public void unzip(InputStream zipFileStream, String extractionPath) throws IOException
	{
		try
		{
			ZipInputStream zin = new ZipInputStream(zipFileStream);
			ZipEntry ze = null;
			while((ze = zin.getNextEntry()) != null)
			{
				if(ze.isDirectory())
				{
					if(!FileHelpers.createFolder(extractionPath + ze.getName()))
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
