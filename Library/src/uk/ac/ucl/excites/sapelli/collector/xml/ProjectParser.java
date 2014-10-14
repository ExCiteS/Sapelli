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

package uk.ac.ucl.excites.sapelli.collector.xml;

import java.io.File;
import java.io.InputStream;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.TransmissionSettings;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.shared.io.UnclosableBufferedInputStream;
import uk.ac.ucl.excites.sapelli.shared.util.xml.DocumentParser;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLAttributes;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLHasher;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;

/**
 * Handler for project (i.e. survey) description XML files
 * 
 * Currently supported formats are the v1.x format and the new v2.x format.
 * Introducing new format version number is only really necessary when major changes are made that affect (backwards & forwards) compatibility.
 * Rules:
 * 	- If a file is parsed which does not mention a format:
 * 		- v1.x is assumed for "pre-Sapelli" projects (which have <ExCiteS-Collector-Project> tag);
 * 		- v2.x (DEFAULT_FORMAT) is assumed for Sapelli projects (with <Sapelli-Collector-Project> tag).
 * 	- If a file is parsed which mentions a higher format than the highest supported one the parser will issue a warning and attempt parsing it as the highest supported one (which could fail).
 *  - If a file is parsed which mentions a lower format than the lowest supported one the parser will throw an exception.
 * 
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public class ProjectParser extends DocumentParser
{

	// STATICS--------------------------------------------------------
	
	static public enum Format
	{
		v0_x, 	// v0.1: trial versions: never used in field & not supported in any present implementation (only really listed here to reserver value 0 such that v1_x corresponds to 1, etc.)
		v1_x, 	// v1.x: "pre-Sapelli" versions of the ExCiteS Data Collection platform (used for AP, OIFLEG & Ethiopia/Jed)
		v2_x	// v2.x: first series of Sapelli-branded versions
		// future releases...
	}
	static public final Format LOWEST_SUPPORTED_FORMAT = Format.v1_x;
	static public final Format HIGHEST_SUPPORTED_FORMAT = Format.values()[Format.values().length - 1]; //last value in enum
	static public final Format DEFAULT_FORMAT = Format.v2_x;
	
	// Tags:
	static private final String TAG_PROJECT = "SapelliCollectorProject";
	static private final String TAG_PROJECT_V1X = "ExCiteS-Collector-Project";

	// Attributes:
	static private final String ATTRIBUTE_PROJECT_FORMAT = "format";
	static private final String ATTRIBUTE_PROJECT_ID = "id";
	static private final String ATTRIBUTE_PROJECT_NAME = "name";
	static private final String ATTRIBUTE_PROJECT_VARIANT = "variant";
	static private final String ATTRIBUTE_PROJECT_VERSION = "version";
	static private final String ATTRIBUTE_PROJECT_START_FORM = "startForm";
	

	// DYNAMICS-------------------------------------------------------
	private final String basePath;
	private final boolean createProjectFolder;
	
	private Format format = DEFAULT_FORMAT;
	private Integer fingerPrint;
	private Project project;
	private String startFormID;
	private HashMap<Relationship, String> relationshipToFormID;
	private HashMap<Relationship, List<ConstraintDescription>> relationshipToConstraints;

	public ProjectParser(String basePath, boolean createProjectFolder)
	{
		super();
		this.basePath = basePath;
		this.createProjectFolder = createProjectFolder;
		this.relationshipToFormID = new HashMap<Relationship, String>();
		this.relationshipToConstraints = new HashMap<Relationship, List<ConstraintDescription>>();
	}

	public Project parseProject(File xmlFile) throws Exception
	{
		return parseProject(open(xmlFile));
	}

	public Project parseProject(InputStream input) throws Exception
	{		
		// (Re)Initialise:
		format = DEFAULT_FORMAT;
		project = null;
		fingerPrint = null;
		startFormID = null;
		relationshipToFormID.clear();
		relationshipToConstraints.clear();
		
		// Get XML hash:
		UnclosableBufferedInputStream ubInput = new UnclosableBufferedInputStream(input); // decorate stream to avoid it from being closed and to ensure we can use mark/reset
		ubInput.mark(Integer.MAX_VALUE);
		fingerPrint = (new XMLHasher()).getJavaHashCode(ubInput);
		ubInput.reset();
		ubInput.makeClosable();
		
		// Parse XML:
		parse(ubInput); //!!!
		return project;
	}

	@Override
	public void startDocument() throws SAXException
	{
		// does nothing (for now)
	}
		
	@Override
	public void endDocument() throws SAXException
	{
		// does nothing (for now)
	}

	@Override
	protected void parseStartElement(String uri, String localName, String qName, XMLAttributes attributes) throws SAXException
	{
		try
		{
			// <Sapelli-Collector-Project>, or <ExCiteS-Collector-Project> (for backwards compatibility)
			if(qName.equals(TAG_PROJECT) || qName.equals(TAG_PROJECT_V1X))
			{
				if(project != null)
				{
					addWarning("Ignoring additional " + TAG_PROJECT + " or " + TAG_PROJECT_V1X + " element.");
					return;
				}
				
				// Detect format version...
				int formatVersion = attributes.getInteger(ATTRIBUTE_PROJECT_FORMAT, qName.equals(TAG_PROJECT_V1X) ? Format.v1_x.ordinal() : DEFAULT_FORMAT.ordinal()); //default: v1.x for ExCiteS tag, DEFAULT_FORMAT for Sapelli tag. 
				// 	too low:
				if(formatVersion < LOWEST_SUPPORTED_FORMAT.ordinal())
					throw new SAXException("Unsupported format version: " + formatVersion);
				// 	too high:
				else if(formatVersion > HIGHEST_SUPPORTED_FORMAT.ordinal())
				{	//issue warning and then try to parse as highest supported format (might fail)
					addWarning("Format version reported in XML file (" + formatVersion + ") is unsupported (" + LOWEST_SUPPORTED_FORMAT + " <= supported <= " + HIGHEST_SUPPORTED_FORMAT + "), attempting parsing with rules for version " + HIGHEST_SUPPORTED_FORMAT); 
					format = HIGHEST_SUPPORTED_FORMAT;
				}
				//	within range (or default because missing attribute):
				else
					format = Format.values()[formatVersion]; 
				
				// Project...
				project = new Project(	(format == Format.v1_x) ?
											Project.PROJECT_ID_V1X_TEMP : // for format = 1 we set a temp id value (will be replaced by Form:schema-id) 
											attributes.getRequiredInteger(qName, ATTRIBUTE_PROJECT_ID, "because format is >= 2"), // id is required for format >= 2
										attributes.getRequiredString(TAG_PROJECT, ATTRIBUTE_PROJECT_NAME, true, false),
										attributes.getString(ATTRIBUTE_PROJECT_VARIANT, null, true, false),
										attributes.getString(ATTRIBUTE_PROJECT_VERSION, Project.DEFAULT_VERSION, true, false),
										fingerPrint,
										basePath,
										createProjectFolder);
				
				// Read startForm ID:
				startFormID = attributes.getString(ATTRIBUTE_PROJECT_START_FORM, null, true, false); 
				
				// Add subtree parsers:
				addSubtreeParser(new ConfigurationParser(this));
				addSubtreeParser(new FormParser(this));
			}
			// <?>
			else
				addWarning("Ignored unrecognised or invalidly placed/repeated element <" + qName + ">.");
		}
		catch(SAXException se)
		{
			throw se;
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			throw new SAXException("Error while parsing element <" + qName + ">: " + e.getMessage(), e);
		}
	}

	@Override
	protected void parseEndElement(String uri, String localName, String qName) throws SAXException
	{
		// </Sapelli-Collector-Project>, or </ExCiteS-Collector-Project> (for backwards compatibility)
		if(qName.equals(TAG_PROJECT) || qName.equals(TAG_PROJECT_V1X))
		{
			clearSubtreeParsers();
						if(project.getTransmissionSettings() == null)
			{
				project.setTransmissionSettings(new TransmissionSettings());
				addWarning("No transmission settings found, defaults are used");
			}
			
			if(project.getForms().size() == 0)
				throw new SAXException("A project such have at least 1 form!");
			else
			{
				// Resolve startForm
				Form startForm = project.getForm(startFormID); // will return null if startFormID is null or there is no form with that name, uses equalsIgnoreCase()
				if(startForm != null)
					project.setStartForm(startForm);
				//else: first form of project will remain the startForm
				
				// Resolve form relationships:
				for(Entry<Relationship, String> entry : relationshipToFormID.entrySet())
				{
					Relationship rel = entry.getKey();
					Form relatedForm = project.getForm(entry.getValue()); // uses equalsIgnoreCase()
					if(relatedForm == null)
						throw new SAXException("Relationship \"" + rel.getID() + "\" in form \"" + rel.getForm().getID() + "\" refers to unknown related form \"" + entry.getValue() + "\".");
					rel.setRelatedForm(relatedForm); // will trigger initialisation of Schema of relatedForm (this should not be a problem, it will not be done again below)
				}
				
				// Initialise forms...
				for(Form form : project.getForms())
				{	
					try
					{
						// generates Schema, Columns & ValueDictionaries:
						form.initialiseStorage();
					}
					catch(ModelFullException e)
					{
						throw new SAXException("This project contains more data-producing Forms than allowed (maximum: " + Project.MAX_RECORD_PRODUCING_FORMS + ").");
					}
					addWarnings(form.getWarnings()); // !!!
				}
				// Seal project model:
				project.getModel().seal();
				
				// Resolve relationship constraints:
				for(Entry<Relationship, List<ConstraintDescription>> entry : relationshipToConstraints.entrySet())
					for(ConstraintDescription constrDesc : entry.getValue())
						try
						{
							Relationship rel = entry.getKey();
							rel.addConstraint(constrDesc.resolve(rel.getRelatedForm()));
						}
						catch(Exception e)
						{
							throw new SAXException("Error upon resolving constraint on Relationship \"" + entry.getKey().getID() + "\"", e);
						}
			}
		}
	}

	/**
	 * @return the format
	 */
	protected Format getFormat()
	{
		return format;
	}

	/**
	 * @return the project
	 */
	protected Project getProject()
	{
		return project;
	}
	
	/**
	 * Called from {@link FormParser}
	 * 
	 * @param relationship
	 * @param formID
	 */
	protected void addRelationship(Relationship relationship, String formID)
	{
		relationshipToFormID.put(relationship, formID);
	}
	
	protected void addRelationshipConstraint(Relationship relationship, String columnName, String comparisonAttrib, String valueString) throws SAXException
	{
		if(!relationshipToConstraints.containsKey(relationship))
			relationshipToConstraints.put(relationship, new ArrayList<ConstraintDescription>());
		// Add constraint description to be resolved later:
		try
		{
			relationshipToConstraints.get(relationship).add(new ConstraintDescription(columnName, comparisonAttrib, valueString));
		}
		catch(ParseException pe)
		{
			throw new SAXException("Error upon parsing constraint", pe);
		}
	}
	
	/**
	 * Helper class to temporarily hold descriptions of constraints (for now only of {@link RuleConstraints}), until they can be resolved to Constraint instances
	 * 
	 * @author mstevens
	 */
	private class ConstraintDescription
	{
		
		String columnName;
		RuleConstraint.Comparison comparison;
		String valueString;
		
		/**
		 * @param columnName
		 * @param comparison
		 * @param valueString
		 * @throws ParseException 
		 */
		public ConstraintDescription(String columnName, String comparisonAttrib, String valueString) throws ParseException
		{
			this.columnName = columnName;
			this.valueString = valueString;
			this.comparison = RuleConstraint.parseComparisonString(comparisonAttrib);
		}
		
		public Constraint resolve(Form form) throws SAXException, IllegalArgumentException, NullPointerException, ParseException
		{
			// Form checks:
			if(form == null)
				throw new NullPointerException("Non-null form is needed to resolve Constraint");
			if(!form.isProducesRecords())
			{
				addWarning("Cannot impose constraint on records of form \"" + form.getID() + "\" because it does not produce data records.");
				return null;
			}
			
			// Get column:
			ColumnPointer columnPointer = new ColumnPointer(form.getSchema(), columnName); // will throw IllegalArgumentException if no such column is found (but name sanitation will be used first)
			
			// Column check:
			if(!(columnPointer.getColumn() instanceof ComparableColumn<?>))
				throw new SAXException("Constraint refers to a column (\"" + columnName + "\") which is not comparable.");
			
			// Return RuleConstraint:
			return RuleConstraint.FromString(columnPointer, comparison, valueString); 
		}
		
	}

}
