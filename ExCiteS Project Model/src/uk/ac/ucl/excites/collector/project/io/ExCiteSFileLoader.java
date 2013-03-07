/**
 * 
 */
package uk.ac.ucl.excites.collector.project.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.FileHelpers;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;

/**
 * Loader for .excites files (which are actually just renamed ZIP files)
 * 
 * @author mstevens
 * 
 */
public class ExCiteSFileLoader
{

	static public final String EXCITES_FILE_EXTENSION = "excites";
	static private final String PROJECT_FILE = "PROJECT.xml";

	public String basePath;

	/**
	 * @param basePath
	 * @throws IOException 
	 */
	public ExCiteSFileLoader(String basePath) throws IOException
	{
		this.basePath = basePath;
		if(!FileHelpers.createFolder(basePath))
			throw new IOException("Base path (" + basePath + ") does not exist and could not be created.");
	}

	public Project load(File excitesFile) throws Exception
	{
		if(excitesFile == null || !excitesFile.exists() || excitesFile.length() == 0)
			throw new IllegalArgumentException("Invalid excites file");
		Project p = null;
		// Parse PROJECT.xml:
		try
		{
			ProjectParser parser = new ProjectParser(basePath, true);
			p = parser.parseProject(getInputStream(excitesFile, PROJECT_FILE));
		}
		catch(Exception e)
		{
			throw new Exception("Error on extracting or parsing " + PROJECT_FILE, e);
		}
		// Extract excites file contents to project path:
		try
		{
			unzip(excitesFile, p.getProjectPath());
		}
		catch(Exception e)
		{
			throw new Exception("Error on extracting contents of " + excitesFile.getName(), e);
		}
		return p;
	}

	private InputStream getInputStream(File zipFile, String filename) throws IOException
	{
		ZipInputStream zin = new ZipInputStream(new FileInputStream(zipFile));
		ZipEntry ze = null;
		while((ze = zin.getNextEntry()) != null)
		{
			if(ze.getName().equalsIgnoreCase(filename))
				return zin; // stream is now positioned to read the indicated file
		}
		throw new IOException(filename + " not found in " + zipFile.getName());
	}

	private void unzip(File zipFile, String extractionPath) throws IOException
	{
		try
		{
			ZipInputStream zin = new ZipInputStream(new FileInputStream(zipFile));
			ZipEntry ze = null;
			while((ze = zin.getNextEntry()) != null)
			{
				System.out.println("Extracting: " + ze.getName());
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
			throw new IOException("Error on unzipping ExCiteS file", e);
		}
	}
}
