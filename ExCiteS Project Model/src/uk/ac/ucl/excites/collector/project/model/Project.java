package uk.ac.ucl.excites.collector.project.model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.util.FileHelpers;

/**
 * @author mstevens
 *
 */
public class Project
{
	
	static public final int DEFAULT_VERSION = 0;
	
	static public final String IMAGE_FOLDER = "img";
	static public final String SOUND_FOLDER = "snd";
	static public final String DATA_FOLDER = "data";
	
	private String name;
	private int version;
	private String projectPath;
	private List<Form> forms;
	
	public Project(String name, String basePath)
	{
		this(name, DEFAULT_VERSION, basePath);
	}
	
	public Project(String name, int version, String basePath)
	{
		if(name == null || name.isEmpty() || basePath == null || basePath.isEmpty())
			throw new IllegalArgumentException("Both a name and a valid path are required");
		this.name = FileHelpers.makeValidFileName(name);
		this.version = version;
		//Path
		if(basePath.charAt(basePath.length() - 1) != File.separatorChar)
			basePath += File.separatorChar;
		this.projectPath = basePath + this.name + File.separatorChar + "v" + version + File.separatorChar;  
		//Forms collection
		this.forms = new ArrayList<Form>();
	}
	
	public void addForm(Form frm)
	{
		forms.add(frm);
	}
	
	public List<Form> getForms()
	{
		return forms;
	}
	
	public String getName()
	{
		return name;
	}
	
	public int getVersion()
	{
		return version;
	}
	
	public String getProjectPath()
	{
		return projectPath;
	}
	
	public String getImagePath()
	{
		return projectPath + IMAGE_FOLDER + File.separator;
	}
	
	public String getSoundPath()
	{
		return projectPath + SOUND_FOLDER + File.separator;
	}
	
	public String getDataPath()
	{
		return projectPath + DATA_FOLDER + File.separator;
	}
	
}

