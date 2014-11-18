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

package uk.ac.ucl.excites.sapelli.collector.load;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.io.FileUtils;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.parse.ProjectParser;
import uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessTask;
import uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.Unzipper;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;

/**
 * Class with methods to load (or just parse) Sapelli projects from .sapelli/.excites/.sap files (which are actually just renamed ZIP files).
 * 
 * @author mstevens, Michalis Vitos
 */
public class ProjectLoader implements WarningKeeper
{
	
	// STATICS -----------------------------------------------------------
	static public final String[] SAPELLI_FILE_EXTENSIONS = { "sap", "sapelli", "excites", "zip" };
	static public final String PROJECT_FILE = "PROJECT.xml";

	/**
	 * Checks if the given file has a support sapelli file extension
	 * 
	 * @param file
	 * @return
	 */
	static public boolean HasSapelliFileExtension(File file)
	{
		String path = file.getAbsolutePath().toLowerCase();
		for(String extention : ProjectLoader.SAPELLI_FILE_EXTENSIONS)
			if(path.endsWith("." + extention))
				return true;
		return false;
	}
	
	/**
	 * @param folderPath path to folder in which the PROJECT.xml file resides
	 * @return a project instance or null in case something went wrong
	 */
	static public Project ParseProject(String folderPath)
	{
		try
		{
			return new ProjectParser().parseProject(new File(folderPath + File.separator + PROJECT_FILE));
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			return null;
		}
	}
	
	/**
	 * @param sapelliFile
	 * @return
	 * @throws Exception
	 */
	static public FileInputStream openStream(File sapelliFile) throws Exception
	{
		if(sapelliFile == null || !sapelliFile.exists() || sapelliFile.length() == 0)
			throw new IllegalArgumentException("Invalid Sapelli file");
		return new FileInputStream(sapelliFile);
	}
	
	// DYNAMICS ----------------------------------------------------------
	/*package*/ final FileStorageProvider fileStorageProvider;
	private final ProjectChecker checker;
	private final PostProcessor postProcessor;
	private List<String> warnings;
	
	private final File tempFolder;
	private final ProjectParser parser;

	/**
	 * @param fileStorageProvider
	 * @throws FileStorageException
	 */
	public ProjectLoader(FileStorageProvider fileStorageProvider) throws FileStorageException
	{
		this(fileStorageProvider, null, null); // no post-processing nor checking
	}
	
	/**
	 * @param fileStorageProvider
	 * @param postProcessor (may be null)
	 * @param checker (may be null)
	 * @throws FileStorageException
	 */
	public ProjectLoader(FileStorageProvider fileStorageProvider, PostProcessor postProcessor, ProjectChecker checker) throws FileStorageException
	{
		if(fileStorageProvider == null)
			throw new NullPointerException("fileStorageProvider cannot be null!");
		this.fileStorageProvider = fileStorageProvider;
		this.postProcessor = postProcessor;
		this.checker = checker;
		
		// Get/create the temp folder:
		tempFolder = fileStorageProvider.getTempFolder(true);
		
		// Create the project folder
		this.parser = new ProjectParser();
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
		return load(openStream(sapelliFile));
	}
	
	/**
	 * Extract the given sapelli file (provided as an InputStream) and parses the PROJECT.xml; returns the resulting Project object.
	 * 
	 * @param sapelliFileInputStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project load(InputStream sapelliFileInputStream) throws Exception
	{
		clearWarnings();
		Project project = null;
		File extractFolder = new File(tempFolder.getAbsolutePath() + File.separator + System.currentTimeMillis());
		try
		{
			// STEP 1 - Extract the content of the Sapelli file to a new subfolder of the temp folder:
			try
			{
				FileHelpers.createDirectory(extractFolder);
				Unzipper.unzip(sapelliFileInputStream, extractFolder);
			}
			catch(Exception e)
			{
				throw new Exception("Error on extracting contents of Sapelli file.", e);
			}
			
			// STEP 2 - Parse PROJECT.xml:
			try
			{	
				project = parser.parseProject(new File(extractFolder.getAbsolutePath() + File.separator + PROJECT_FILE));
			}
			catch(Exception e)
			{
				throw new Exception("Error on parsing " + PROJECT_FILE, e);
			}
			// Copy parser warnings:
			addWarnings(parser.getWarnings());
			
			// STEP 3 - Check if project is acceptable:
			checkProject(project); // throws IllegalArgumentException if something is wrong

			// STEP 4 - Create move extracted files to project folder:
			try
			{
				File installFolder = fileStorageProvider.getProjectInstallationFolder(project, true);
				FileHelpers.moveDirectory(extractFolder, installFolder);
				extractFolder = installFolder;
			}
			catch(Exception e)
			{
				throw new Exception("Error on moving extracted files to project folder.", e);
			}
			
			// STEP 5 - Run post-processing tasks:
			List<PostProcessTask> tasks = parser.getPostProcessingTasks();
			if(!tasks.isEmpty())
			{
				if(postProcessor != null)
					for(PostProcessTask task : tasks)
					{
						try
						{
							task.execute(postProcessor, project, this);
						}
						catch(Exception e)
						{
							throw new Exception("Error on executing post-processing task", e);
						}
					}
				else
					addWarning("Unable to perform " + tasks.size() + " post-processing");
			}
		}
		catch(Exception e)
		{
			// Delete temp or install folder:
			FileUtils.deleteQuietly(extractFolder);
			// Re-throw Exception:
			throw e;
		}
		
		// Return project object:
		return project;
	}
	
	/**
	 * @param project
	 * @throws IllegalArgumentException when the project is not acceptable
	 */
	protected void checkProject(Project project) throws IllegalArgumentException
	{
		if(checker != null)
			checker.checkProject(project); // throws IllegalArgumentException if something is wrong
	}
	
	/**
	 * Parses the PROJECT.xml present in the given sapelli file (provided as a File object), without extracting the contents to storage and without executing load tasks; returns the resulting Project object.
	 * 
	 * @param sapelliFile
	 * @return the loaded Project
	 * @throws Exception
	 */

	public Project loadParseOnly(File sapelliFile) throws Exception
	{
		if(sapelliFile == null || !sapelliFile.exists() || sapelliFile.length() == 0)
			throw new IllegalArgumentException("Invalid Sapelli file");
		return loadParseOnly(new FileInputStream(sapelliFile));
	}

	/**
	 * Parses the PROJECT.xml present in the given sapelli file (provided as an InputStream), without extracting the contents to storage and without executing load tasks; returns the resulting Project object.
	 * 
	 * @param sapelliFileStream
	 * @return the loaded Project
	 * @throws Exception
	 */
	public Project loadParseOnly(InputStream sapelliFileStream) throws Exception
	{
		clearWarnings();
		try
		{	// Parse PROJECT.xml:
			Project project = parser.parseProject(Unzipper.getInputStreamForFileInZip(sapelliFileStream, PROJECT_FILE));
			// Copy parser warnings:
			addWarnings(parser.getWarnings());
			// Check if project is acceptable:
			checkProject(project); // throws IllegalArgumentException if something is wrong
			// all OK:
			return project;
		}
		catch(Exception e)
		{
			throw new Exception("Error on parsing " + PROJECT_FILE, e);
		}
	}
	
	@Override
	public void addWarning(String warning)
	{
		if(warnings == null)
			warnings = new ArrayList<String>();
		warnings.add(warning);
	}

	@Override
	public void addWarnings(Collection<String> warnings)
	{
		if(this.warnings == null)
			this.warnings = new ArrayList<String>();
		this.warnings.addAll(warnings);
	}

	@Override
	public List<String> getWarnings()
	{
		return warnings != null ? warnings : Collections.<String> emptyList();
	}
	
	@Override
	public void clearWarnings()
	{
		warnings = null;
	}
	
	/**
	 * Callback interface for checking Project acceptance
	 * 
	 * @author mstevens
	 */
	public interface ProjectChecker
	{

		/**
		 * @param loadedProject
		 * @throws IllegalArgumentException if something is wrong
		 */
		public void checkProject(Project loadedProject) throws IllegalArgumentException;
		
	}
	
}
