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

import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
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
	
	// STATICS-------------------------------------------------------
	public static String DOWNLOADS_SAPELLI_FOLDER = "Sapelli";
	public static String BACKUP_FILE = "Backup";
	
	// Folders to be used by Sapelli
	public static enum Folder
	{
		/**
		 * Folder for (live, i.e. non-dumped/backed-up) databases and auxiliary files (e.g. journal file)
		 */
		DB,
		
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
	
	// Subfolders of project installation folder:
	static public final String IMAGE_FOLDER = "img";
	static public final String SOUND_FOLDER = "snd";
	
	// DYNAMICS------------------------------------------------------
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
		// Use FileHelpers.createFolder() to check if folder exists, and create it if it doesn't:
		if(FileHelpers.createDirectory(sapelliFolder) && sapelliFolder.canRead())
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
			case DB: 
				return getDBFolder(create);
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
	 * @param projDescr
	 * @param create
	 * @return
	 * @throws FileStorageException
	 */
	protected File getProjectSpecificSubFolder(File parent, ProjectDescriptor projDescr, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(parent, projDescr.getName(), projDescr.getVariant(), projDescr.getVersion(), create);
	}
	
	/**
	 * @param parent
	 * @param projectName
	 * @param projectVariant - null and empty String are both treated as 'no variant specified'
	 * @param projectVersion
	 * @param create
	 * @return
	 * @throws FileStorageException
	 */
	protected File getProjectSpecificSubFolder(File parent, String projectName, String projectVariant, String projectVersion, boolean create) throws FileStorageException
	{
		// Hierarchy: (PARENT]/(projectName)[ (projectVariant)]/v(projectVersion)
		return	getSubFolder(	getSubFolder(	parent,
												FileHelpers.makeValidFileName(projectName + (projectVariant != null && !projectName.isEmpty() ? " " + projectVariant : "")),
												create),
								FileHelpers.makeValidFileName("v" + projectVersion),
								create);
	}
	
	public File getProjectsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Projects.name(), create);
	}

	public File getProjectInstallationFolder(ProjectDescriptor projDescr, boolean create) throws FileStorageException
	{	
		return getProjectSpecificSubFolder(getProjectsFolder(create), projDescr, create);
	}
	
	public File getProjectInstallationFolder(String projectName, String projectVariant, String projectVersion, boolean create) throws FileStorageException
	{	
		return getProjectSpecificSubFolder(getProjectsFolder(create), projectName, projectVariant, projectVersion, create);
	}
	
	/**
	 * Returns and creates a Sapelli-specific subfolder of the device/system Download folder
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
	
	public File getDBFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.DB.name(), create);
	}
	
	public File getCrashFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Crashes.name(), create);
	}
	
	public File getTempFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Temp.name(), create);
	}
	
	/**
	 * Creates a folder with the given name inside of the Sapelli Temp folder
	 * 
	 * @param name
	 * @return
	 * @throws FileStorageException
	 */
	public File getTempSubFolder(String name) throws FileStorageException
	{
		return getSubFolder(getTempFolder(true), name, true);
	}
	
	public File getExportFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliDownloadsFolder(), Folder.Export.name(), create);
	}

	public File getAttachmentsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Attachments.name(), create);
	}
	
	public File getProjectAttachmentFolder(ProjectDescriptor projDescr, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(getAttachmentsFolder(create), projDescr, create);
	}
	
	public File getLogsFolder(boolean create) throws FileStorageException
	{
		return getSubFolder(getSapelliFolder(), Folder.Logs.name(), create);
	}
	
	public File getProjectLogsFolder(ProjectDescriptor projDescr, boolean create) throws FileStorageException
	{
		return getProjectSpecificSubFolder(getLogsFolder(create), projDescr, create);
	}
	
	/**
	 * @param projDescr
	 * @param imageFileRelativePath
	 * @return file object (pointing to a file which does *not* necessarily exist), or null if the given path was null or empty
	 */
	public File getProjectImageFile(ProjectDescriptor projDescr, String imageFileRelativePath)
	{
		try
		{
			if(imageFileRelativePath == null || imageFileRelativePath.isEmpty())
				return null;
			return new File(getProjectInstallationFolder(projDescr, false).getAbsolutePath() + File.separator + IMAGE_FOLDER + File.separator + imageFileRelativePath);
		}
		catch(FileStorageException fse)
		{
			fse.printStackTrace(System.err);
			return null;
		}		
	}
	
	/**
	 * @param projDescr
	 * @param soundFileRelativePath
	 * @return file object (pointing to a file which does *not* necessarily exist), or null if the given path was null or empty
	 */
	public File getProjectSoundFile(ProjectDescriptor projDescr, String soundFileRelativePath)
	{
		try
		{
			if(soundFileRelativePath == null || soundFileRelativePath.isEmpty())
				return null;
			return new File(getProjectInstallationFolder(projDescr, false).getAbsolutePath() + File.separator + SOUND_FOLDER + File.separator + soundFileRelativePath);
		}
		catch(FileStorageException fse)
		{
			fse.printStackTrace(System.err);
			return null;
		}
	}
	
	/**
	 * Returns a File object representing a (still uncreated) ZIP file which will be used for a Collector back-up.
	 * The path will be: <device_download_folder>/Sapelli/Backup_timestamp.zip
	 * 
	 * @return the ZIP file (not yet created!)
	 * @throws FileStorageException
	 */
	public File getNewBackupFile() throws FileStorageException
	{
		return new File(getSapelliDownloadsFolder(), BACKUP_FILE + "_" + TimeUtils.getTimestampForFileName() + "." + Zipper.ZIP_EXTENSION);
	}

	/**
	 * @param parentFolder
	 * @param subFolderName
	 * @param create
	 * @return
	 * @throws FileStorageException
	 */
	private File getSubFolder(File parentFolder, String subFolderName, boolean create) throws FileStorageException
	{
		try
		{
			return FileHelpers.getSubDirectory(parentFolder, subFolderName, create);
		}
		catch(Exception e)
		{	// Wrap as FileStorageException:
			throw new FileStorageException(e);
		}
	}
	
}
