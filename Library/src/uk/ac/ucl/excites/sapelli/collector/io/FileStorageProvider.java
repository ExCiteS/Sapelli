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

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.Zipper;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;

/**
 * Class which to manages (almost) all path generation/resolving and folder creation for different types of file storage needs
 * 
 * @author mstevens, Michalis Vitos
 */
public class FileStorageProvider
{
	public static String DOWNLOADS_SAPELLI_FOLDER = "Sapelli";
	public static String BACKUP_FILE = "Backup";
	
	// Folders to be used by Sapelli
	public static enum Folder
	{
		/**
		 * Folder for (media) attachments, files produced during data collection (e.g. photos, audio, video), grouped per project
		 */
		Attachments,
		
		/**
		 * Folder for downloads
		 */
		Downloads,
		
		/**
		 * Folder in which stacktraces are placed
		 */
		Crashes,
		
		/**
		 * Folder for record exports
		 */
		Export,

		/**
		 * Folder for log files, both project-specific and general
		 */
		Logs,

		/**
		 * Folder in which projects are installed
		 */
		Projects,
		
		/**
		 * Folder for temporary files
		 */
		Temp
	}

	private final File sapelliFolder;
	private final File downloadsFolder;
	
	public FileStorageProvider(File sapelliFolder, File downloadsFolder)
	{
		if(sapelliFolder == null)
			throw new NullPointerException("SapelliFolder cannot be null!");
		if(!sapelliFolder.exists() || !sapelliFolder.isDirectory())
			throw new FileStorageException("No an existing directory (" + sapelliFolder.getAbsolutePath() + ")");
		this.sapelliFolder = sapelliFolder;

		if(downloadsFolder == null)
			throw new NullPointerException("DownloadsFolder cannot be null!");
		if(!downloadsFolder.exists() || !downloadsFolder.isDirectory())
			throw new FileStorageException("No an existing directory (" + downloadsFolder.getAbsolutePath() + ")");
		this.downloadsFolder = downloadsFolder;
	}
	
	/**
	 * @return the root Sapelli folder
	 * @throws FileStorageException
	 */
	public File getSapelliFolder() throws FileStorageException
	{
		if(sapelliFolder.exists() && sapelliFolder.canRead())
			return sapelliFolder;
		else
			throw new FileStorageException("Sapelli folder is not or no longer accessible (path: " + sapelliFolder.getAbsolutePath());
	}
	
	/**
	 * @return absolute path to the root Sapelli folder, including trailing file separator (/ or \)
	 */
	public String getSapelliFolderPath() throws FileStorageException
	{
		return getSapelliFolder().getAbsolutePath() + File.separator;
	}

	/**
	 * Return a File for a given folder type.
	 * 
	 * @param folderType
	 *            the type of storage file to return.
	 * @return
	 */
	public File getFolder(Folder folderType, boolean create)
	{
		switch(folderType)
		{
			case Attachments:
				return getAttachmentsFolder(create);
	
			case Downloads:
				return getSapelliDownloadsFolder();
	
			case Crashes:
				return getCrashFolder(create);
	
			case Export:
				return getExportFolder(create);
	
			case Logs:
				return getLogsFolder(create);
	
			case Projects:
				return getProjectsFolder(create);
	
			case Temp:
				return getTempFolder(create);
	
			default:
				return null;
		}
	}

	/**
	 * @return absolute path to Sapelli folder for a given type, including trailing file separator (/ or \)
	 */
	public String getFolderPath(Folder folderType, boolean create)
	{
		File folder = getFolder(folderType, create);
		return folder != null ? folder.getAbsolutePath() + File.separator : null;
	}

	/**
	 * @param parent
	 * @param project
	 * @param create
	 * @return
	 * @throws FileStorageException
	 */
	protected File getProjectSpecificSubFolder(File parent, Project project, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(parent, project.getName(), project.getVariant(), project.getVersion(), create);
	}
	
	/**
	 * @param parent
	 * @param projectName
	 * @param projectVariant
	 * @param projectVersion
	 * @param create
	 * @return
	 * @throws FileStorageException
	 */
	protected File getProjectSpecificSubFolder(File parent, String projectName, String projectVariant, String projectVersion, boolean create) throws FileStorageException
	{
		// Hierarchy: (PARENT]/(projectName)[ (projectVariant)]/v(projectVersion)
		return	getSubFolder(	getSubFolder(	parent,
												FileHelpers.makeValidFileName(projectName + (projectVariant != null ? " " + projectVariant : "")),
												create),
								FileHelpers.makeValidFileName("v" + projectVersion),
								create);
	}
	
	public File getProjectsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Projects.name(), create);
	}

	public File getProjectInstallationFolder(Project project, boolean create) throws FileStorageException
	{	
		return getProjectSpecificSubFolder(getProjectsFolder(create), project, create);
	}
	
	public File getProjectInstallationFolder(String projectName, String projectVariant, String projectVersion, boolean create) throws FileStorageException
	{	
		return getProjectSpecificSubFolder(getProjectsFolder(create), projectName, projectVariant, projectVersion, create);
	}
	
	/**
	 * Returns and creates a Sapelli-specific subfolder of the device/system Downloads folder
	 * 
	 * @return
	 * @throws FileStorageException
	 */
	public File getSapelliDownloadsFolder() throws FileStorageException
	{
		if(downloadsFolder.exists() && downloadsFolder.canWrite())
			return getSubFolder(downloadsFolder, DOWNLOADS_SAPELLI_FOLDER, true);
		else
			throw new FileStorageException("Downloads folder is not or no longer accessible (path: " + downloadsFolder.getAbsolutePath());
	}
	
	public File getCrashFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Crashes.name(), create);
	}
	
	public File getTempFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Temp.name(), create);
	}
	
	public File getExportFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliDownloadsFolder(), Folder.Export.name(), create);
	}

	public File getAttachmentsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Attachments.name(), create);
	}
	
	public File getProjectAttachmentFolder(Project project, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(getAttachmentsFolder(create), project, create);
	}
	
	public File getLogsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Logs.name(), create);
	}
	
	public File getProjectLogsFolder(Project project, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(getLogsFolder(create), project, create);
	}
	
	/**
	 * @return the location for creating a zip with backup data i.e. Downloads/Sapelli/Backup_timestamp.zip
	 */
	public File getBackupFile()
	{
		return new File(getSapelliDownloadsFolder(), BACKUP_FILE + "_" + TimeUtils.getTimestampForFileName() + "." + Zipper.ZIP_EXTENSION);
	}

	private File getSubFolder(File parentFolder, String subFolderName, boolean create) throws FileStorageException
	{
		File subFolder = new File(parentFolder, subFolderName);
		
		if(create)
		{	// Create and test the subfolder
			if(!FileHelpers.createFolder(subFolder))
				throw new FileStorageException("Could not create folder: " + subFolder.getAbsolutePath());
		}
		return subFolder;
	}
	
}
