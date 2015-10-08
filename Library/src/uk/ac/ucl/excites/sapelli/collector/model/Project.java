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

package uk.ac.ucl.excites.sapelli.collector.model;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.diagnostics.HeartbeatSchema;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * A Sapelli Collector Project (i.e. a survey consisting of one or more forms)
 * 
 * @author mstevens
 */
public class Project extends ProjectDescriptor
{
	
	//STATICS-------------------------------------------------------------
	static public final String DEFAULT_DEFAULT_LANGUAGE = "en"; // the default "default language" to set if there isn't one specified (English)
	
	static public final boolean DEFAULT_LOGGING = true;
		
	static public final int MAX_FORMS = 32;
	
	static public final int MAX_RECORD_PRODUCING_FORMS = Math.min(MAX_FORMS, Model.MAX_SCHEMATA - 1 /* subtract 1 for the heartbeatSchema */);

	//DYNAMICS------------------------------------------------------------
	private String defaultLanguage;
	private boolean logging;
	private Schema heartbeatSchema;
	private final List<Form> forms;
	private Model model;
	private Form startForm;
	
	/**
	 * @param id
	 * @param name
	 * @param variant
	 * @param version
	 * @param fingerPrint - hash code computed against XML (ignoring comments and whitespace; see XMLHasher) 
	 */
	public Project(int id, String name, String variant, String version, int fingerPrint)
	{
		super(id, name, variant, version, fingerPrint);
		
		// Forms list:
		this.forms = new ArrayList<Form>();
		// Project language (for TTV):
		this.defaultLanguage = DEFAULT_DEFAULT_LANGUAGE;
		// Logging:
		this.logging = DEFAULT_LOGGING;
	}
	
	/**
	 * @param id
	 */
	@Override
	protected void initialise(int id)
	{
		super.initialise(id); // !!!
		
		// Initialise Model (important this should remain last):
		this.model = new Model(CollectorClient.GetModelID(this), this.toString(false).replaceAll(" ", "_"));
		
		// Heartbeat schema (Important: never put this before the model initialisation!):
		this.heartbeatSchema = new HeartbeatSchema(this); // will add itself to the model
	}
	
	/**
	 * Add a {@link Form} to the project
	 * 
	 * @param frm
	 * @throws IllegalStateException when the maximum number of forms is reached
	 */
	public void addForm(Form frm) throws IllegalStateException
	{
		if(forms.size() == MAX_FORMS)
			throw new IllegalStateException("Maximum number of forms reached");
		forms.add(frm);
		if(forms.size() == 1) //first form becomes startForm by default
			startForm = frm;
	}
	
	public int getNumberOfForms()
	{
		return forms.size();
	}
	
	public List<Form> getForms()
	{
		return Collections.unmodifiableList(forms);
	}
	
	public Model getModel()
	{
		return model;
	}
	
	/**
	 * @return the heartbeatSchema
	 */
	public Schema getHeartbeatSchema()
	{
		return heartbeatSchema;
	}

	/**
	 * @param position
	 * @return	the {@link Form} with the specified {@code position}, or {@code null} if the project has no such form. 
	 */
	public Form getForm(int position)
	{
		if(position >= 0 && position < forms.size())
			return forms.get(position);
		else
			return null; // no such form
	}
	
	/**
	 * @param id
	 * @return	the {@link Form} with the specified {@code id}, or {@code null} if the project has no such form.
	 */
	public Form getForm(String id)
	{
		for(Form f : forms)
			if(f.id.equalsIgnoreCase(id)) // form IDs are treated as case insensitive
				return f;
		return null; // no such form
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
	 * @param currentForm
	 * @return the form following the given one
	 */
	public Form getNextForm(Form currentForm)
	{
		if(currentForm.getPosition() + 1 < forms.size())
			return forms.get(currentForm.getPosition() + 1); // go to next field in the form
		else
			return null;
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
	
	/**
	 * @return the BCP 47 format string representing the currently set default language for this project
	 */
	public String getDefaultLanguage()
	{
		return defaultLanguage;
	}
	
	/**
	 * Set the project's default language (the language that will be used for text-to-speech synthesis unless a language is specified in the current form)
	 * @param defaultLanguage the language to set, as a valid BCP 47 format string (e.g. "en-GB")
	 */
	public void setDefaultLanguage(String defaultLanguage)
	{
		this.defaultLanguage = defaultLanguage;
	}
	
	/**
	 * Find all files used by the project
	 * 
	 * @param fileStorageProvider to resolve relative paths
	 * @return a set of files that the project depends on
	 */
	public Set<File> getFiles(FileStorageProvider fileStorageProvider)
	{
		Set<File> filesSet = new HashSet<File>();
		for(Form form : forms)
			form.addFiles(filesSet, fileStorageProvider);
		return filesSet;
	}
	
	/**
	 * @return a list of files (as File object) that are referred to by (forms of) this project but which could not be found or accessed   
	 */
	public List<File> getMissingFiles(FileStorageProvider fileStorageProvider)
	{
		SortedSet<File> missingFiles = new TreeSet<File>();
		for(File file : getFiles(fileStorageProvider))
			if(file != null && (!file.isFile() || !file.exists() || !file.canRead()))
				missingFiles.add(file);
		// Return as list:
		if(missingFiles.isEmpty())
			return Collections.<File> emptyList();
		else
			return Arrays.asList(missingFiles.toArray(new File[missingFiles.size()]));
	}
	
	/**
	 * Generate list of paths (relative to the project path) of files that are referred to by (forms of) this project but which could not be found or accessed
	 * 
	 * @return a list relative paths of missing files, or null if no files are missing   
	 */
	public List<String> getMissingFilesRelativePaths(FileStorageProvider fileStorageProvider)
	{
		List<File> missingFiles = getMissingFiles(fileStorageProvider);
		if(missingFiles.isEmpty())
			return Collections.<String> emptyList();
		List<String> missingFilePaths = new ArrayList<String>(missingFiles.size());
		int startIndex = fileStorageProvider.getProjectInstallationFolder(this, false).getAbsolutePath().length() + 1; // +1 for file separator
		for(File missingFile : getMissingFiles(fileStorageProvider))
			missingFilePaths.add(missingFile.getAbsolutePath().substring(startIndex));
		return missingFilePaths;
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj != null && obj.getClass() == Project.class)
		{
			Project that = (Project) obj;
			return 	super.equalDescription(that) &&
					// no need to check the model here
					(this.defaultLanguage != null ? this.defaultLanguage.equals(that.defaultLanguage) : that.defaultLanguage == null) &&
					this.logging == that.logging &&
					this.forms.equals(that.forms) &&
					(this.startForm != null ? this.startForm.equals(that.startForm) : that.startForm == null) &&
					this.v1xSchemaVersion == that.v1xSchemaVersion;
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // ProjectDescriptor#hashCode()
		// Do not include the model here
		hash = 31 * hash + (defaultLanguage == null ? 0 : defaultLanguage.hashCode());
		hash = 31 * hash + (logging ? 0 : 1);
		hash = 31 * hash + forms.hashCode();
		hash = 31 * hash + (startForm == null ? 0 : startForm.hashCode());
		hash = 31 * hash + (v1xSchemaVersion == null ? -1 : v1xSchemaVersion);
		return hash;
	}
	
}

