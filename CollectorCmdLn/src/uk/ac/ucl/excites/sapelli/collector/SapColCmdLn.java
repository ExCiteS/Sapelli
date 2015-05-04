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

package uk.ac.ucl.excites.sapelli.collector;

import java.io.File;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * Simple command-line interface to load/verify (and in the future store) Sapelli Collector projects
 * 
 * @author mstevens
 */
public class SapColCmdLn
{

	static private Options options = new Options();
	static
	{
		options.addOption("p", true, "Sapelli working directory");
		Option loadFile = Option.builder("load").hasArg().argName("sap_file").desc("Sapelli project (*.sap) to load").build();
		options.addOption(loadFile);
		options.addOption("json", false, "Produce JSON output");
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception
	{
		File workingDir = new File(System.getProperty("user.dir"));
		
		CommandLineParser parser = new DefaultParser();
		CommandLine cmd = null;
		try
		{
			// parse the command line arguments
			cmd = parser.parse(options, args);
		}
		catch(ParseException exp)
		{
			// oops, something went wrong
			System.err.println("Parsing failed.  Reason: " + exp.getMessage());
			System.exit(1);
		}

		File baseFolder;
		if(cmd.hasOption("p"))
			baseFolder = new File(cmd.getOptionValue("p")); // TODO trim? remove quotes? typed arg?
		else
			baseFolder = new File(workingDir, "Sapelli");
		if(!baseFolder.exists())
			baseFolder.mkdir();
		File dlFolder = new File(baseFolder, "downloads");
		if(!dlFolder.exists())
			dlFolder.mkdir();
		FileStorageProvider fsp = new FileStorageProvider(baseFolder, dlFolder);

		// Setup database(s)
		// CollectorClient sapClient = new CollectorClient();
		// RecordStore recStore = new JavaSQLiteRecordStore(sapClient, DB_FOLDER, "ImportAndQueryTest");
		// //new DB4ORecordStore(new SapelliCollectorClient(projStore), DB4O_FOLDER, "ImportAndQueryTest");
		// ProjectStore projStore = new ProjectRecordStore(recStore, fsp);
		// sapClient.setProjectStore(projStore); // !!!

		if(cmd.hasOption("load"))
		{
			File sapFile = new File(cmd.getOptionValue("load"));
			if(!sapFile.exists())
				sapFile = new File(workingDir, cmd.getOptionValue("load"));
			
			ProjectLoader loader = new ProjectLoader(fsp);
			Project project = null;
			try
			{
				project = loader.load(sapFile);
				if(!cmd.hasOption("json"))
					printProjectInfo(sapFile, project);
				else
					printProjectInfoJSON(sapFile, project);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				System.exit(2);
			}
		}
	}
	
	static public void printProjectInfo(File sapFile, Project project)
	{
		System.out.println("Project info:");
		System.out.println(" - loaded from: " + sapFile.getAbsolutePath());
		System.out.println(" - id: " + project.getID());
		System.out.println(" - fingerprint: " + project.getFingerPrint());
		System.out.println(" - name: " + project.getName());
		System.out.println(" - variant: " + project.getVariant());
		System.out.println(" - version: " + project.getVersion());
		System.out.println(" - Model id: " + project.getModel().id);
		System.out.println(" - Forms:");
		int f = 0;
		for(Form frm : project.getForms())
		{
			System.out.println("    * Form " + ++f + " info:");
			System.out.println("       - id/name: " + frm.id);
			System.out.println("       - producesData: " + frm.isProducesRecords());
			System.out.println("       - Model schema number: " + (frm.isProducesRecords() ? frm.getSchema().getModelSchemaNumber() : "N/A"));
		}
	}
	
	static public void printProjectInfoJSON(File sapFile, Project project)
	{
		// TODO ...
	}

}