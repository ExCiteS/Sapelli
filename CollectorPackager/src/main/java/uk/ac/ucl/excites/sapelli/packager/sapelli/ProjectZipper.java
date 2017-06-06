package uk.ac.ucl.excites.sapelli.packager.sapelli;

import org.jetbrains.annotations.NotNull;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;


/**
 * Created by Michalis on 05/06/2017.
 */
@Slf4j
public class ProjectZipper
{
	public static final String SAP_EXTENSION = "sap";

	private static final int BUFFER_SIZE = 2048;
	private ProjectChecker projectChecker;

	public ProjectZipper(ProjectChecker projectChecker)
	{
		// this class should never be instantiated
		this.projectChecker = projectChecker;
	}

	public void zipProject() throws IOException
	{
		// Add Files:
		List<File> filesToZip = new ArrayList<>();
		filesToZip.add(projectChecker.getProjectXml());
		filesToZip.add(projectChecker.getProjectImg());
		filesToZip.add(projectChecker.getProjectSnd());
		filesToZip.add(projectChecker.getProjectResources());

		Zip(getSapFile(), filesToZip);
	}

	/**
	 * Get the sap file in a format PROJECT_NAME vVERSION.sap i.e. Test v1.0.sap
	 *
	 * @return sap file
	 */
	@NotNull
	private File getSapFile()
	{
		// Get Project
		final Project project = projectChecker.getProject();

		// Get root dir and ensure it is a directory path
		String rootFileName = projectChecker.getSapelliProjectDir().getAbsoluteFile().toString();
		rootFileName = FileHelpers.ensureDirectoryPath(rootFileName);

		// Create name
		final String sapFileName = rootFileName
		  + project.getName()
		  + " v" + project.getVersion()
		  + "."
		  + SAP_EXTENSION;

		return new File(sapFileName);
	}

	/**
	 * Zips the given list of files/folders to create the a zip archive file at the given path
	 *
	 * @param zipDestinationPath
	 * @param sourceFiles
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	static public void Zip(String zipDestinationPath, List<File> sourceFiles) throws FileNotFoundException, IOException
	{
		// Make sure the zip file ends with the appropriate extension
		if(!FileHelpers.getFileExtension(zipDestinationPath).equalsIgnoreCase(SAP_EXTENSION))
			zipDestinationPath += "." + SAP_EXTENSION;

		Zip(new File(zipDestinationPath), sourceFiles);
	}

	/**
	 * Zips the given list of files/folders to create the given destination zip archive file
	 *
	 * @param zipDestination the destination file, if it already exists it will be overwritten
	 * @param sourceFiles
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	static public void Zip(File zipDestination, List<File> sourceFiles) throws FileNotFoundException, IOException
	{
		// Create containing folder:
		if(!zipDestination.exists())
			FileHelpers.createParentDirectory(zipDestination);

		ZipOutputStream zipOutputStream = null;
		try
		{
			// Create the ZipOutputStream
			zipOutputStream = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipDestination, false)));

			// Loop through all source files/folder and add them to the zip file
			if(sourceFiles != null)
				for(File sourceFile : sourceFiles)
					if(sourceFile != null && sourceFile.exists())
					{
						int basePathLength = sourceFile.getParentFile().getAbsolutePath().length() + 1; // +1 to include final slash
						if(sourceFile.isDirectory())
							zipFolder(zipOutputStream, sourceFile, basePathLength);
						else
							zipFile(zipOutputStream, sourceFile, basePathLength);
					}
		}
		finally
		{
			if(zipOutputStream != null)
				try
				{    // Close the ZipOutputStream
					zipOutputStream.close();
				}
				catch(Exception ignore)
				{
				}
		}
	}

	/**
	 * Zips a file
	 *
	 * @param zipOutputStream
	 * @param file
	 * @param basePathLength
	 * @throws IOException
	 */
	static private void zipFile(ZipOutputStream zipOutputStream, File file, int basePathLength) throws IOException
	{
		BufferedInputStream source = null;
		try
		{
			byte[] data = new byte[BUFFER_SIZE];
			source = new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE);
			zipOutputStream.putNextEntry(new ZipEntry(file.getAbsolutePath().substring(basePathLength))); // use path relative to basePath
			int count;
			while((count = source.read(data, 0, BUFFER_SIZE)) != -1)
				zipOutputStream.write(data, 0, count);
		}
		finally
		{
			if(source != null)
				try
				{
					source.close();
				}
				catch(Exception ignore)
				{
				}
		}
	}

	/**
	 * Zips a (sub)folder
	 *
	 * @param zipOutputStream
	 * @param folder
	 * @param basePathLength
	 * @throws IOException
	 */
	private static void zipFolder(ZipOutputStream zipOutputStream, File folder, int basePathLength) throws IOException
	{
		for(File file : folder.listFiles())
			if(file.isDirectory())
				zipFolder(zipOutputStream, file, basePathLength);
			else
				zipFile(zipOutputStream, file, basePathLength);
	}

	public static String getRelativePath(File sourceDir, File file)
	{
		// Trim off the start of source dir path...
		String path = file.getPath().substring(sourceDir.toString().length());
		if(path.startsWith(File.pathSeparator))
		{
			path = path.substring(1);
		}
		return path;
	}
}

