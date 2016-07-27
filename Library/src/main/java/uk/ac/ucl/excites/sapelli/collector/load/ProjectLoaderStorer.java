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

package uk.ac.ucl.excites.sapelli.collector.load;

import java.io.File;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.FileUtils;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectDuplicateException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;

/**
 * Class which uses {@link ProjectLoader} to load Sapelli projects (from .sapelli/.excites/.sap files) and
 * {@link ProjectStore} to store the resulting {@link Project} objects. 
 * 
 * @author mstevens
 */
public class ProjectLoaderStorer implements ProjectLoader.ProjectChecker, WarningKeeper
{
	
	protected final ProjectLoader loader; 
	protected final ProjectStore projectStore;
	
	/**
	 * @param fileStorageProvider
	 * @param projectStore
	 */
	public ProjectLoaderStorer(FileStorageProvider fileStorageProvider, ProjectStore projectStore)
	{
		this(fileStorageProvider, projectStore, null); // no post-processing
	}
	
	/**
	 * @param fileStorageProvider
	 * @param projectStore
	 */
	public ProjectLoaderStorer(FileStorageProvider fileStorageProvider, ProjectStore projectStore, PostProcessor postProcessor)
	{
		this.loader = new ProjectLoader(fileStorageProvider, postProcessor, this);
		this.projectStore = projectStore;
	}
	
	private Project store(Project project) throws Exception
	{
		try
		{
			return projectStore.add(project);
		}
		catch(Exception e) // this shouldn't happen, but just in case...
		{
			FileUtils.deleteQuietly(loader.fileStorageProvider.getProjectInstallationFolder(project, false));
			throw e;
		}
	}
	
	/**
	 * Loads a Sapelli project from a sapelli file (provided as a File object) and, if it passes all checks, stores it in the ProjectStore.
	 * "Loading" includes extracting the sapelli file contents contents, parsing PROJECT.xml to construct a Project object, installing project files & running post-loading tasks.
	 * 
	 * @param sapelliFile
	 * @return the {@link Project} object
	 * @throws Exception
	 */
	public Project loadAndStore(File sapelliFile) throws Exception
	{
		return store(loader.load(sapelliFile));
	}
	
	/**
	 * Loads a Sapelli project from a sapelli file (provided as an InputStream) and, if it passes all checks, stores it in the ProjectStore.
	 * "Loading" includes extracting the sapelli file contents contents, parsing PROJECT.xml to construct a Project object, installing project files & running post-loading tasks.
	 * 
	 * @param sapelliFileInputStream
	 * @return the {@link Project} object
	 * @throws Exception
	 */
	public Project loadAndStore(InputStream sapelliFileInputStream) throws Exception
	{
		return store(loader.load(sapelliFileInputStream));
	}
	
	/**
	 * Loads a Sapelli project from a sapelli file (provided as a File object) and, if it passes all checks, stores it in the ProjectStore.
	 * "Loading" includes extracting the sapelli file contents contents, parsing PROJECT.xml to construct a Project object, installing project files & running post-loading tasks.
	 * Success or failure is reported using the provided callback (if not null).
	 * 
	 * @param sapelliFile
	 * @param sourceURI
	 * @param callback (may be null)
	 */
	public void loadAndStore(File sapelliFile, String sourceURI, FileSourceCallback callback)
	{
		Project project = null;		
		try
		{
			project = loadAndStore(sapelliFile);
		}
		catch(Exception e)
		{	// Report failure:
			if(callback != null)
				callback.projectLoadStoreFailure(sapelliFile, sourceURI, e);
		}
		// Report success:
		if(callback != null)
			callback.projectLoadStoreSuccess(sapelliFile, sourceURI, project, getWarnings());
	}
	
	/**
	 * Loads a Sapelli project from a sapelli file (provided as an InputStream) and, if it passes all checks, stores it in the ProjectStore.
	 * "Loading" includes extracting the sapelli file contents contents, parsing PROJECT.xml to construct a Project object, installing project files & running post-loading tasks.
	 * Success or failure is reported using the provided callback (if not null).
	 * 
	 * @param sapelliFileInputStream
	 * @param sourceURI
	 * @param callback (may be null)
	 */
	public void loadAndStore(InputStream sapelliFileInputStream, StreamSourceCallback callback)
	{
		Project project = null;		
		try
		{
			project = loadAndStore(sapelliFileInputStream);
		}
		catch(Exception e)
		{	// Report failure:
			if(callback != null)
				callback.projectLoadStoreFailure(e);
		}
		// Report success:
		if(callback != null)
			callback.projectLoadStoreSuccess(project, getWarnings());
	}
	
	@Override
	public void addWarning(String warning)
	{
		loader.addWarning(warning);
	}

	@Override
	public void addWarnings(Collection<String> warnings)
	{
		loader.addWarnings(warnings);
	}

	@Override
	public List<String> getWarnings()
	{
		return loader.getWarnings();
	}
	
	@Override
	public void clearWarnings()
	{
		loader.clearWarnings();
	}

	/**
	 * @param loadedProject
	 * @throws ProjectDuplicateException
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader.ProjectChecker#checkProject(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void checkProject(Project loadedProject) throws ProjectDuplicateException
	{
		projectStore.duplicateCheck(loadedProject);
	}
	
	/**
	 * Super interface for FileSourceCallback & StreamSourceCallback
	 * 
	 * @author mstevens
	 */
	public interface Callback
	{
		// empty
	}
	
	/**
	 * Callback methods for loading projects from a File
	 * 
	 * @author mstevens
	 */
	public interface FileSourceCallback extends Callback
	{
		
		/**
		 * @param sapelliFile
		 * @param sourceURI
		 * @param project
		 * @param warnings a list of warnings, possibly empty but never null
		 */
		public void projectLoadStoreSuccess(File sapelliFile, String sourceURI, Project project, List<String> warnings);
		
		/**
		 * @param sapelliFile
		 * @param sourceURI
		 * @param cause
		 */
		public void projectLoadStoreFailure(File sapelliFile, String sourceURI, Exception cause);
		
	}
	
	/**
	 * Callback methods for loading projects from a Stream
	 * 
	 * @author mstevens
	 */
	public interface StreamSourceCallback extends Callback
	{
		
		/**
		 * @param project
		 * @param warnings a list of warnings, possibly empty but never null
		 */
		public void projectLoadStoreSuccess(Project project, List<String> warnings);
		
		/**
		 * @param sapelliFile
		 * @param sourceURI
		 * @param cause
		 */
		public void projectLoadStoreFailure(Exception cause);
		
	}
	
}
