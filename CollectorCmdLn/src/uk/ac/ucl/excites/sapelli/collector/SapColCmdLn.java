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
import java.io.IOException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.GeoKeyFormDescriber;

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
		options.addOption("geokey", false, "Produce 'sapelli_project_info' (JSON) for geokey_sapelli");
	}

	static FileStorageProvider fsp;
	
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
		fsp = new FileStorageProvider(baseFolder, new File(System.getProperty("java.io.tmpdir")));

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
				if(cmd.hasOption("json"))
					printProjectInfoJSON(sapFile, project);
				else if(cmd.hasOption("geokey"))
					printProjectInfoForGeoKey(sapFile, project);
				else
					printProjectInfo(sapFile, project);
					
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				System.exit(2);
			}
		}
		
		System.exit(0);
	}

	static public void printProjectInfo(File sapFile, Project project)
	{
		System.out.println("Project info:");
		System.out.println(" - source: " + sapFile.getAbsolutePath());
		System.out.println(" - id: " + project.getID());
		System.out.println(" - fingerprint: " + project.getFingerPrint());
		System.out.println(" - name: " + project.getName());
		System.out.println(" - variant: " + project.getVariant());
		System.out.println(" - version: " + project.getVersion());
		System.out.println(" - display name: " + project.toString(false));
		System.out.println(" - model id: " + project.getModel().id);
		System.out.println(" - install-path: " + fsp.getProjectInstallationFolder(project, false).getAbsolutePath());
		System.out.println(" - Forms:");
		int f = 0;
		for(Form frm : project.getForms())
		{
			System.out.println("    * Form " + ++f + " info:");
			System.out.println("       - id: " + frm.id);
			System.out.println("       - producesData: " + frm.isProducesRecords());
			System.out.println("       - model schema number: " + (frm.isProducesRecords() ? frm.getSchema().getModelSchemaNumber() : "n/a"));
		}
	}

	static public void printProjectInfoJSON(File sapFile, Project project) throws IOException
	{
		// Create the node factory that gives us nodes.
		JsonNodeFactory factory = new JsonNodeFactory(false);

		// create a json factory to write the treenode as json. for the example
		// we just write to console
		JsonFactory jsonFactory = new JsonFactory();
		JsonGenerator generator = jsonFactory.createGenerator(System.out);
		ObjectMapper mapper = new ObjectMapper();

		// the root node
		ObjectNode projectJSON = factory.objectNode();
		
		// describe project:
		projectJSON.put("source", sapFile.getAbsolutePath());
		projectJSON.put("id", project.getID());
		projectJSON.put("fingerprint", project.getFingerPrint());
		projectJSON.put("name", project.getName());
		projectJSON.put("variant", project.getVariant());
		projectJSON.put("version", project.getVersion());
		projectJSON.put("display-name", project.toString(false));
		projectJSON.put("model-id", project.getModel().id);
		projectJSON.put("install-path", fsp.getProjectInstallationFolder(project, false).getAbsolutePath());
		ArrayNode formsJSON = factory.arrayNode();
		for(Form frm : project.getForms())
		{
			ObjectNode formJSON = factory.objectNode();
			formJSON.put("id", frm.id);
			formJSON.put("produces-data", frm.isProducesRecords());
			formJSON.put("model-schema-number", (frm.isProducesRecords() ? frm.getSchema().getModelSchemaNumber() : null));
			formsJSON.add(formJSON);
		}
		projectJSON.set("forms", formsJSON);
		
		// Serialise:
		mapper.writeTree(generator, projectJSON);
	}	

	/**
	 * @param sapFile
	 * @param project
	 * @throws IOException
	 * @see https://github.com/ExCiteS/geokey-sapelli
	 */
	static public void printProjectInfoForGeoKey(File sapFile, Project project) throws IOException
	{
		GeoKeyFormDescriber gkFormDescriber = new GeoKeyFormDescriber();
		
		// Create the node factory that gives us nodes.
		JsonNodeFactory factory = new JsonNodeFactory(false);

		// create a json factory to write the treenode as json. for the example
		// we just write to console
		JsonFactory jsonFactory = new JsonFactory();
		JsonGenerator generator = jsonFactory.createGenerator(System.out);
		ObjectMapper mapper = new ObjectMapper();

		// the root node
		ObjectNode projectJSON = factory.objectNode();
		
		// describe project:
		projectJSON.put("name", project.getName());
		projectJSON.put("variant", project.getVariant());
		projectJSON.put("version", project.getVersion());
		projectJSON.put("geokey_project_name", project.toString(false));
		projectJSON.put("sapelli_id", project.getID());
		projectJSON.put("sapelli_fingerprint", project.getFingerPrint());
		projectJSON.put("sapelli_model_id", project.getModel().id);
		projectJSON.put("installation_path", fsp.getProjectInstallationFolder(project, false).getAbsolutePath());
		ArrayNode formsJSON = factory.arrayNode();
		for(Form frm : project.getForms())
		{
			ObjectNode formNode = gkFormDescriber.getFormJSON(frm);
			if(formNode != null)
				formsJSON.add(formNode);
		}
		projectJSON.set("forms", formsJSON);
		
		// Serialise:
		mapper.writeTree(generator, projectJSON);
	}
	
}