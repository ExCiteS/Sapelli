/**
 * 
 */
package uk.ac.ucl.excites.collector.project.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.util.FileHelpers;

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

	private String tempFolderPath;
	private ProjectParser parser;
	
	/**
	 * @param basePath
	 * @throws IOException 
	 */
	public ExCiteSFileLoader(String projectsFolderPath, String tempFolderPath) throws IOException
	{
		this.tempFolderPath = FileHelpers.ensureFolderPath(tempFolderPath);
		if(!FileHelpers.createFolder(tempFolderPath))
			throw new IOException("Temp folder (" + this.tempFolderPath + ") does not exist and could not be created.");
		String projPath = FileHelpers.ensureFolderPath(projectsFolderPath);
		if(!FileHelpers.createFolder(projPath))
			throw new IOException("Projects folder (" + projPath + ") does not exist and could not be created.");
		this.parser = new ProjectParser(projPath, true);
	}

	/**
	 * Extract the given excites file (provided as a File object) and parses the PROJECT.xml; returns the resulting Project object.
	 * 
	 * @param excitesFile
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project load(File excitesFile) throws Exception
	{
		if(excitesFile == null || !excitesFile.exists() || excitesFile.length() == 0)
			throw new IllegalArgumentException("Invalid excites file");
		return load(new FileInputStream(excitesFile));
	}
	
	/**
	 * Extract the given excites file (provided as an InputStream) and parses the PROJECT.xml; returns the resulting Project object.
	 * 
	 * @param excitesFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project load(InputStream excitesFileStream) throws Exception
	{
		Project p = null;
		String extractFolderPath = tempFolderPath + System.currentTimeMillis() + File.separatorChar;
		// Extract the content of the excites file to a new subfolder of the temp folder:
		try
		{
			FileHelpers.createFolder(extractFolderPath);
			unzip(excitesFileStream, extractFolderPath);
		}
		catch(Exception e)
		{
			throw new Exception("Error on extracting contents of excites file.", e);
		}
		// Parse PROJECT.xml:
		try
		{	
			p = parser.parseProject(new File(extractFolderPath + PROJECT_FILE));
		}
		catch(Exception e)
		{
			throw new Exception("Error on parsing " + PROJECT_FILE, e);
		}
		// Create move extracted files to project folder:
		try
		{
			File extractFolder = new File(extractFolderPath);
			//Move files:
			for(File f : extractFolder.listFiles())
				f.renameTo(new File(p.getProjectFolderPath() + f.getName()));
			//Delete extract folder:
			extractFolder.delete();
		}
		catch(Exception e)
		{
			throw new Exception("Error on moving extracted files to project folder.", e);
		}
		return p;
	}

	/**
	 * Parses the PROJECT.xml present in the given excites file (provided as a File object), without extracting the contents to storage; returns the resulting Project object.
	 * 
	 * @param excitesFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project loadWithoutExtract(File excitesFile) throws Exception
	{
		if(excitesFile == null || !excitesFile.exists() || excitesFile.length() == 0)
			throw new IllegalArgumentException("Invalid excites file");
		return loadWithoutExtract(new FileInputStream(excitesFile));
	}

	/**
	 * Parses the PROJECT.xml present in the given excites file (provided as an InputStream), without extracting the contents to storage; returns the resulting Project object.
	 * 
	 * @param excitesFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project loadWithoutExtract(InputStream excitesFileStream) throws Exception
	{
		try
		{	// Parse PROJECT.xml:
			return parser.parseProject(getInputStreamForFileInZip(excitesFileStream, PROJECT_FILE));
		}
		catch(Exception e)
		{
			throw new Exception("Error on parsing " + PROJECT_FILE, e);
		}
	}
	
	private void unzip(InputStream zipFileStream, String extractionPath) throws IOException
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
			throw new IOException("Error on unzipping ExCiteS file", e);
		}
	}
	
	private InputStream getInputStreamForFileInZip(InputStream zipFileStream, String filename) throws IOException
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
	
	public List<String> getParserWarnings()
	{
		return parser.getWarnings();
	}
	
}
