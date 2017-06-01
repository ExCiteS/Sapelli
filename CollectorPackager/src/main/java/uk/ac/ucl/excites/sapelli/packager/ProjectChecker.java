package uk.ac.ucl.excites.sapelli.packager;

import java.io.File;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

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
	private final File project_xml;
	private final File project_img;
	private final File project_snd;
	private final File project_resources;

	public ProjectChecker(File sapelliProjectDir)
	{
		// Get the PROJECT.xml file
		this.sapelliProjectDir = sapelliProjectDir;
		project_xml = ProjectLoader.GetProjectXMLFile(sapelliProjectDir);
		project_img = new File(sapelliProjectDir, FileStorageProvider.IMAGE_FOLDER);
		project_snd = new File(sapelliProjectDir, FileStorageProvider.SOUND_FOLDER);
		project_resources = new File(sapelliProjectDir, FileStorageProvider.RES_FOLDER);

		if(projectXmlExists())
		{
			fsp = new FileStorageProvider(sapelliProjectDir, new File(System.getProperty("java.io.tmpdir")));
			projectLoader = new ProjectLoader(fsp);

			try
			{
				project = projectLoader.loadProjectFile(project_xml);
			}
			catch(Exception e)
			{
				log.error("Error while loading the project:", e);
			}
		}
	}


	public boolean sapelliProjectDirExists()
	{
		return sapelliProjectDir != null && sapelliProjectDir.exists();
	}

	public boolean projectXmlExists()
	{
		return project_xml.exists();
	}

	public boolean projectImgExists()
	{
		return project_img.exists();
	}

	public boolean projectSndExists()
	{
		return project_snd.exists();
	}

	public boolean projectResourcesExists()
	{
		return project_resources.exists();
	}
}
