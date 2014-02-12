package uk.ac.ucl.excites.sapelli.collector.project.model;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.Settings;
import uk.ac.ucl.excites.sapelli.util.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.util.io.FileWriter;

/**
 * @author mstevens
 *
 */
public class Project
{
	
	//STATICS-------------------------------------------------------------	
	static public final int PROJECT_ID_SIZE = Schema.V1X_SCHEMA_ID_SIZE; // = 24 bits (kept the same was the v1.x Schema#id, for backwards compatibility)
	static public final IntegerRangeMapping PROJECT_ID_FIELD = IntegerRangeMapping.ForSize(0, PROJECT_ID_SIZE); // unsigned 24bit integer (compatible with old schemaID)
	
	static public final String DEFAULT_VERSION = "0";
	
	static public final int PROJECT_HASH_SIZE = Schema.SCHEMA_USAGE_ID_SIZE; // = 32 bits
	static public final IntegerRangeMapping PROJECT_HASH_FIELD = IntegerRangeMapping.ForSize(0, PROJECT_HASH_SIZE); // unsigned(!) 32bit integer
	
	static public final int MAX_FORMS = ((int) Schema.SCHEMA_USAGE_SUB_ID_FIELD.getHighBound()) + 1; // = 15 + 1 = 16
	
	// Backwards compatibility:
	static public final int PROJECT_ID_V1X_TEMP = -1;
	
	// Subfolders:
	static public final String IMAGE_FOLDER = "img";
	static public final String SOUND_FOLDER = "snd";
	static public final String DATA_FOLDER = "data";
	static public final String TEMP_FOLDER = "temp";
	static public final String LOG_FOLDER = "logs"; //subfolder of data/
	static public final String DOCS_FOLDER = "docs";
	
	static public final String NO_MEDIA_FILE = ".nomedia"; //Info: http://www.makeuseof.com/tag/hide-private-picture-folders-gallery-android
	
	static public final boolean DEFAULT_LOGGING = false;
	
	//DYNAMICS------------------------------------------------------------
	private int id = Integer.MIN_VALUE; //don't init to 0 because that is an acceptable project id
	private long hash;
	private String name;
	private String variant;
	private String version;
	
	private String projectPath;
	
	private Settings transmissionSettings;
	private boolean logging;
	private List<Form> forms;
	private Form startForm;
	
	// For backwards compatibility:
	private boolean v1xProject = false;
	private int schemaVersion = -1;
	
	public Project(int id, long hash, String name, String basePath)
	{
		this(id, hash, name, DEFAULT_VERSION, basePath, false);
	}
	
	public Project(int id, long hash, String name, String version, String basePath, boolean createSubfolder)
	{
		if(name == null || name.isEmpty() || basePath == null || basePath.isEmpty())
			throw new IllegalArgumentException("Both a name and a valid path are required");
		
		// Project id:
		if(id == PROJECT_ID_V1X_TEMP)
		{	//Backwards compatibility
			this.id = id;
			v1xProject = true;
		}
		else
			setID(id); // checks if it fits in field
		
		// Project hash:
		if(PROJECT_HASH_FIELD.fits(hash))
			this.hash = hash;
		else
			throw new IllegalArgumentException("Invalid schema ID, valid values are " + PROJECT_ID_FIELD.getLogicalRangeString() + ".");		
		
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
	
	/**
	 * Must be stay private!
	 * 
	 * @param id
	 */
	private void setID(int id)
	{
		if(PROJECT_ID_FIELD.fits(id))
			this.id = id;
		else
			throw new IllegalArgumentException("Invalid schema ID, valid values are " + PROJECT_ID_FIELD.getLogicalRangeString() + ".");
	}
	
	/**
	 * Backwards compatibility:
	 * 	- project id will be set to schema id of 1st (and assumed only) form
	 *  - schema version of that form will be stored in schemaVersion variable (to be able to parse old record export xml files)
	 * 
	 * @param schemaID
	 * @param schemaVersion
	 */
	public void setSchema(int schemaID, int schemaVersion)
	{
		if(!v1xProject)
			throw new IllegalStateException("Only allowed for v1.x projects (created with id=-1).");
		setID(schemaID);
		if(Schema.V1X_SCHEMA_VERSION_FIELD.fits(schemaVersion))
			this.schemaVersion = schemaVersion;
		else
			throw new IllegalArgumentException("Invalid schema version, valid values are " + Schema.V1X_SCHEMA_VERSION_FIELD.getLogicalRangeString() + ".");
	}
	
	/**
	 * @return the id
	 */
	public int getID()
	{
		return id;
	}

	/**
	 * @return the hash
	 */
	public long getHash()
	{
		return hash;
	}

	public String getName()
	{
		return name;
	}
	
	/**
	 * @return the variant
	 */
	public String getVariant()
	{
		return variant;
	}

	/**
	 * @param variant the variant to set
	 */
	public void setVariant(String variant)
	{
		if(this.variant != null)
			throw new IllegalStateException("Variant cannot be changed after it has been set.");
		if(!"".equals(variant))
			this.variant = variant;
	}

	public String getVersion()
	{
		return version;
	}
	
	/**
	 * @return the v1xProject
	 */
	public boolean isV1xProject()
	{
		return v1xProject;
	}

	/**
	 * @return the schemaVersion
	 */
	public int getSchemaVersion()
	{
		if(!v1xProject)
			throw new IllegalStateException("Only supported for v1.x projects.");
		return schemaVersion;
	}
	
	/**
	 * Add a {@link Form} to the project
	 * 
	 * @param frm
	 */
	public void addForm(Form frm)
	{
		if(forms.size() >= MAX_FORMS)
			throw new IllegalStateException("Project cannot hold more than " + MAX_FORMS + " forms.");
		forms.add(frm);
		if(forms.size() == 1) //first form becomes startForm by default
			startForm = frm;
	}
	
	public List<Form> getForms()
	{
		return forms;
	}
	
	/**
	 * @param index
	 * @return	the {@link Form} with the specified {@code index}, or {@code null} if the project has no such form. 
	 */
	public Form getForm(int index)
	{
		if(index >= 0 && index < forms.size())
			return forms.get(index);
		else
			return null; //no such form
	}
	
	/**
	 * @param id
	 * @return	the {@link Form} with the specified {@code id}, or {@code null} if the project has no such form.
	 */
	public Form getForm(String id)
	{
		for(Form f : forms)
			if(f.getID().equals(id))
				return f;
		return null; //no such form
	}
	
	/**
	 * @return the startForm
	 */
	public Form getStartForm()
	{
		return startForm;
	}

	/**
	 * @param startForm the startForm to set
	 */
	public void setStartForm(Form startForm)
	{
		if(forms.contains(startForm))
			this.startForm = startForm;
		else
			throw new IllegalArgumentException("Unknown form.");
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
	 * @param imageFileRelativePath
	 * @return file object, or null if the given path was null or empty
	 */
	public File getImageFile(String imageFileRelativePath)
	{
		if(imageFileRelativePath == null || imageFileRelativePath.isEmpty())
			return null;
		return new File(getImageFolderPath() + imageFileRelativePath);
	}
	
	/**
	 * @param soundFileRelativePath
	 * @return file object, or null if the given path was null or empty
	 */
	public File getSoundFile(String soundFileRelativePath)
	{
		if(soundFileRelativePath == null || soundFileRelativePath.isEmpty())
			return null;
		return new File(getSoundFolderPath() + soundFileRelativePath);
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
		return name + (variant != null ? (" " + variant) : "") + (version != DEFAULT_VERSION ? " (v" + version + ")" : "");
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
	 * @return a list of files (as paths relative to the project path) that are referred to by (forms of) this project but which could not be found or accessed   
	 */
	public List<String> checkForInvalidFiles()
	{
		List<String> invalidFiles = new ArrayList<String>();
		for(Form form : forms)
			for(File file : form.getFiles(this))
				if(!file.isFile() || !file.exists() || !file.canRead())
					invalidFiles.add(file.getAbsolutePath().substring(projectPath.length()));
		return invalidFiles;
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

