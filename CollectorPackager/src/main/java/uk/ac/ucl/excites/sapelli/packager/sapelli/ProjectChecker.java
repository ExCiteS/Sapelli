package uk.ac.ucl.excites.sapelli.packager.sapelli;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;

/**
 * Class to check if a directory has a valid Sapelli project
 * <p>
 * Created by Michalis on 26/05/2017.
 */
@Slf4j
public class ProjectChecker
{
	@Getter
	private File sapelliProjectDir;
	@Getter
	private Project project;
	private FileStorageProvider fsp;
	private ProjectLoader projectLoader;

	// Sapelli Structure Files:
	@Getter
	private File projectXml;
	@Getter
	private File projectImg;
	@Getter
	private File projectSnd;
	@Getter
	private File projectResources;

	/**
	 * Constructor for a {@link ProjectChecker}
	 *
	 * @param sapelliProjectDir The directory to check for a valid Sapelli project
	 */
	public ProjectChecker(File sapelliProjectDir)
	{
		init(sapelliProjectDir);
	}

	private void init(File sapelliProjectDir)
	{
		this.sapelliProjectDir = sapelliProjectDir;
		// Get the PROJECT.xml file
		projectXml = ProjectLoader.GetProjectXMLFile(sapelliProjectDir);
		projectImg = new File(sapelliProjectDir, FileStorageProvider.IMAGE_FOLDER);
		projectSnd = new File(sapelliProjectDir, FileStorageProvider.SOUND_FOLDER);
		projectResources = new File(sapelliProjectDir, FileStorageProvider.RES_FOLDER);

		if(projectXmlExists())
		{
			fsp = new FileStorageProvider(sapelliProjectDir, new File(System.getProperty("java.io.tmpdir")));
			projectLoader = new ProjectLoader(fsp);

			try
			{
				project = projectLoader.loadProjectFile(projectXml);
			}
			catch(Exception e)
			{
				log.error("Error while loading the project:", e);
			}
		}
	}

	/**
	 * Refresh the Project Checker and recalculate eveyrthing
	 */
	public void refresh()
	{
		init(sapelliProjectDir);
	}

	/**
	 * Check whether the directory provided, to check for a Sapelli project, exists.
	 *
	 * @return true if exists
	 */
	public boolean sapelliProjectDirExists()
	{
		return sapelliProjectDir != null && sapelliProjectDir.exists();
	}

	/**
	 * Check if the PROJECT.XML exists.
	 *
	 * @return true if exists
	 */
	public boolean projectXmlExists()
	{
		return projectXml.exists();
	}

	/**
	 * Check if the /img directory exists.
	 *
	 * @return true if exists
	 */
	public boolean projectImgExists()
	{
		return projectImg.exists();
	}

	/**
	 * Check if the /snd directory exists.
	 *
	 * @return true if exists
	 */
	public boolean projectSndExists()
	{
		return projectSnd.exists();
	}

	/**
	 * Check if the /resources directory exists.
	 *
	 * @return true if exists
	 */
	public boolean projectResourcesExists()
	{
		return projectResources.exists();
	}

	/**
	 * Get a list of the available Warnings, while trying to load the PROJECT.xml
	 *
	 * @return the list of Warnings
	 */
	public List<String> getWarnings()
	{
		return projectLoader.getWarnings();
	}

	/**
	 * Get a list of the available Errors, while trying to load the PROJECT.xml
	 *
	 * @return the list of Errors
	 */
	public List<String> getErrors()
	{
		return projectLoader.getErrors();
	}

	/**
	 * Get a list of paths of files that are referred to by (forms of)
	 * this project but which could not be found or accessed
	 *
	 * @return list of Paths
	 */
	public List<String> getMissingFiles()
	{
		List<String> missing = new ArrayList<>();
		try
		{
			File installationDir = fsp.getProjectInstallationFolder(project, false);
			final Set<File> files = project.getFiles(fsp);

			// Sapelli uses the installation dir to store the imgs/snds/resources etc.
			// Here we clean the installation dir, thus this:
			// -- C:\Users\Michalis\Desktop\Working Dir\Projects\GPS Test 5m\v3.0\img\Back.png
			// will become this:
			// -- C:\Users\Michalis\Desktop\Working Dir\img\Back.png
			for(File file : files)
			{
				// Remove installation dir from file
				String relativePath = FileHelpers.getRelativePath(installationDir, file);
				File missingFile = new File(sapelliProjectDir + relativePath);

				log.info("Check if '{}' is missing.", missingFile);

				// Check if the file exists or it is missing:
				if(!missingFile.exists() || !missingFile.isFile())
					missing.add(missingFile.toString());
			}
		}
		catch(Exception e)
		{
			log.error("Cannot get missing files: ", e);
		}

		return missing;
	}
}
