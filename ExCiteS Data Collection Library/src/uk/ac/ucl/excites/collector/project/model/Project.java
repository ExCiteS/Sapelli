package uk.ac.ucl.excites.collector.project.model;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.util.FileHelpers;
import uk.ac.ucl.excites.util.FileWriter;

/**
 * @author mstevens
 *
 */
public class Project
{
	
	static public final String NO_MEDIA_FILE = ".nomedia"; //Info: http://www.makeuseof.com/tag/hide-private-picture-folders-gallery-android
	
	static public final String DEFAULT_VERSION = "0";
	
	static public final String IMAGE_FOLDER = "img";
	static public final String SOUND_FOLDER = "snd";
	static public final String DATA_FOLDER = "data";
	static public final String TEMP_FOLDER = "temp";
	static public final String LOG_FOLDER = "logs"; //subfolder of data/
	static public final String DOCS_FOLDER = "docs";
	
	static public final boolean DEFAULT_LOGGING = false;
	
	private String name;
	private String version;
	private String projectPath;
	private Settings transmissionSettings;
	private boolean logging;
	private List<Form> forms;
	
	public Project(String name, String basePath) throws IOException
	{
		this(name, DEFAULT_VERSION, basePath, false);
	}
	
	public Project(String name, String version, String basePath, boolean createSubfolder)
	{
		if(name == null || name.isEmpty() || basePath == null || basePath.isEmpty())
			throw new IllegalArgumentException("Both a name and a valid path are required");
		this.name = FileHelpers.makeValidFileName(name);
		this.version = version;
		// Path:
		if(basePath.charAt(basePath.length() - 1) != File.separatorChar)
			basePath += File.separatorChar;
		this.projectPath = basePath;
		if(createSubfolder)
		{
			this.projectPath += this.name + File.separatorChar + "v" + version + File.separatorChar;
			if(!FileHelpers.createFolder(projectPath))
				throw new IllegalArgumentException("Could not create folder: " + projectPath);
			// Create .nomedia file:
			try
			{
				(new File(projectPath + NO_MEDIA_FILE)).createNewFile();
			}
			catch(IOException ignore) {}
		}
		// Forms collection:
		this.forms = new ArrayList<Form>();
		// Logging:
		this.logging = DEFAULT_LOGGING;
	}
	
	public void addForm(Form frm)
	{
		forms.add(frm);
	}
	
	public List<Form> getForms()
	{
		return forms;
	}
	
	/**
	 * @return the transmissionSettings
	 */
	public Settings getTransmissionSettings()
	{
		return transmissionSettings;
	}

	/**
	 * @param transmissionSettings the transmissionSettings to set
	 */
	public void setTransmissionSettings(Settings transmissionSettings)
	{
		this.transmissionSettings = transmissionSettings;
	}

	public String getName()
	{
		return name;
	}
	
	public String getVersion()
	{
		return version;
	}
	
	public String getProjectFolderPath()
	{
		return projectPath;
	}
	
	public String getImageFolderPath()
	{
		return projectPath + IMAGE_FOLDER + File.separator;
	}
	
	public String getSoundFolderPath()
	{
		return projectPath + SOUND_FOLDER + File.separator;
	}
	
	public String getDataFolderPath()
	{
		return projectPath + DATA_FOLDER + File.separator;
	}
	
	/**
	 * @return File object pointing to the data folder for this project
	 * @throws IOException - when the folder cannot be created or is not writable
	 */
	public File getDataFolder() throws IOException
	{
		File folder = new File(getDataFolderPath());
		checkFolder(folder);
		return folder;
	}
	
	public String getTempFolderPath()
	{
		return projectPath + TEMP_FOLDER + File.separator;
	}
	
	/**
	 * @return File object pointing to the temp folder for this project
	 * @throws IOException - when the folder cannot be created or is not writable
	 */
	public File getTempFolder() throws IOException
	{
		File folder = new File(getTempFolderPath());
		checkFolder(folder);
		return folder;
	}
	
	public String toString()
	{
		return name + (version != DEFAULT_VERSION ? " (v" + version + ")" : "");
	}
	
	private void checkFolder(File folder) throws IOException
	{
		// Check if data path is accessible
		if(!FileHelpers.createFolder(folder))
			throw new IOException("Data path (" + folder.getAbsolutePath() + ") cannot be created.");
		if(!folder.canWrite())
			throw new IOException("Data path (" + folder.getAbsolutePath() + ") is not writable.");
	}

	/**
	 * @return the logging
	 */
	public boolean isLogging()
	{
		return logging;
	}

	/**
	 * @param logging the logging to set
	 */
	public void setLogging(boolean logging)
	{
		this.logging = logging;
	}
	
	public String getLogFolderPath() throws IOException
	{
		return getDataFolder().getAbsolutePath() + File.separator + LOG_FOLDER + File.separator;
	}
	
	public File getLogFolder() throws IOException
	{
		File folder = new File(getLogFolderPath());
		checkFolder(folder);
		return folder;
	}
	
	public String getDocsFolderPath()
	{
		return projectPath + DOCS_FOLDER + File.separator;
	}
	
	/**
	 * @return File object pointing to the docs folder for this project
	 * @throws IOException - when the folder cannot be created or is not writable
	 */
	public File getDocsFolder() throws IOException
	{
		File folder = new File(getDocsFolderPath());
		checkFolder(folder);
		return folder;
	}
	
	/**
	 * For now this only generates CSV files that document the indexed values for ChoiceFields
	 * 
	 * @throws IOException
	 */
	public void generateDocumentation() throws IOException
	{
		File docsFolder = getDocsFolder();

		for(Form form : forms)
		{
			for(Field field : form.getFields())
			{
				if(!field.isNoColumn() && field instanceof ChoiceField)
				{					
					FileWriter writer = new FileWriter(docsFolder.getAbsolutePath() + File.separator + form.getName() + "_" + field.getID() + ".csv");
					writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
					writer.write(((ChoiceField) field).getDictionary().toCSV(";"));
					writer.close();
				}
			}
		}
		
	}
	
}

