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

package uk.ac.ucl.excites.sapelli.collector.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.xml.ProjectParser;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.Unzipper;

/**
 * Loader for .sapelli (or .excites or .sap) files, which are actually just renamed ZIP files
 * 
 * @author mstevens, Michalis Vitos
 * 
 */
public class ProjectLoader
{
	
	static public final String[] SAPELLI_FILE_EXTENSIONS = { "excites", "sapelli", "sap" };
	static public final String PROJECT_FILE = "PROJECT.xml";

	private ProjectLoaderClient client;
	private String tempFolderPath;
	private ProjectParser parser;
	
	/**
	 * @param basePath
	 * @throws IOException
	 */
	public ProjectLoader(ProjectLoaderClient client, String projectsFolderPath, String tempFolderPath) throws IOException
	{
		this.client = client;
		// Create the temp folder
		this.tempFolderPath = FileHelpers.ensureFolderPath(tempFolderPath);
		if(!FileHelpers.createFolder(tempFolderPath))
			throw new IOException("Temp folder (" + this.tempFolderPath + ") does not exist and could not be created.");
		// Create .nomedia file:
		(new File(tempFolderPath + Project.NO_MEDIA_FILE)).createNewFile();
		// Create the project folder
		String projPath = FileHelpers.ensureFolderPath(projectsFolderPath);
		if(!FileHelpers.createFolder(projPath))
			throw new IOException("Projects folder (" + projPath + ") does not exist and could not be created.");
		this.parser = new ProjectParser(projPath, true);
	}

	/**
	 * Extract the given sapelli file (provided as a File object) and parses the PROJECT.xml; returns the resulting Project object.
	 * 
	 * @param sapelliFile
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project load(File sapelliFile) throws Exception
	{
		if(sapelliFile == null || !sapelliFile.exists() || sapelliFile.length() == 0)
			throw new IllegalArgumentException("Invalid Sapelli file");
		return load(new FileInputStream(sapelliFile));
	}
	
	/**
	 * Extract the given sapelli file (provided as an InputStream) and parses the PROJECT.xml; returns the resulting Project object.
	 * 
	 * @param sapelliFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project load(InputStream sapelliFileStream) throws Exception
	{
		Project p = null;
		String extractFolderPath = tempFolderPath + System.currentTimeMillis() + File.separatorChar;
		// Extract the content of the Sapelli file to a new subfolder of the temp folder:
		try
		{
			FileHelpers.createFolder(extractFolderPath);
			Unzipper.unzip(sapelliFileStream, extractFolderPath);
		}
		catch(Exception e)
		{
			throw new Exception("Error on extracting contents of Sapelli file.", e);
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
			FileHelpers.moveDirectory(new File(extractFolderPath), new File(p.getProjectFolderPath()));
		}
		catch(Exception e)
		{
			throw new Exception("Error on moving extracted files to project folder.", e);
		}
		return p;
	}

	/**
	 * Parses the PROJECT.xml present in the given sapelli file (provided as a File object), without extracting the contents to storage; returns the resulting Project object.
	 * 
	 * @param sapelliFile
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project loadWithoutExtract(File sapelliFile) throws Exception
	{
		if(sapelliFile == null || !sapelliFile.exists() || sapelliFile.length() == 0)
			throw new IllegalArgumentException("Invalid Sapelli file");
		return loadWithoutExtract(new FileInputStream(sapelliFile));
	}

	/**
	 * Parses the PROJECT.xml present in the given sapelli file (provided as an InputStream), without extracting the contents to storage; returns the resulting Project object.
	 * 
	 * @param sapelliFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project loadWithoutExtract(InputStream sapelliFileStream) throws Exception
	{
		try
		{	// Parse PROJECT.xml:
			return parser.parseProject(Unzipper.getInputStreamForFileInZip(sapelliFileStream, PROJECT_FILE));
		}
		catch(Exception e)
		{
			throw new Exception("Error on parsing " + PROJECT_FILE, e);
		}
	}
	
	public List<String> getParserWarnings()
	{
		return parser.getWarnings();
	}
	
}
