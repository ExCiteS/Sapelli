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

package uk.ac.ucl.excites.sapelli.storage.eximport.xml;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.commons.io.Charsets;
import org.apache.commons.lang3.StringEscapeUtils;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.text.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.SimpleExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExportHelper;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.util.UnexportableRecordsException;


/**
 * Class to export {@link Record}s to XML files, which can be re-imported by {@link XMLRecordsImporter}.
 * 
 * @author mstevens
 */
public class XMLRecordsExporter extends SimpleExporter
{

	// STATIC -------------------------------------------------------
	static public final String FILE_EXTENSION = "xml";
	
	static public final String TAG_RECORDS_EXPORT = "RecordsExport";
	
	/**
	 * Whether to use XML v1.0 ({@code false}) or XML v1.1 ({@code true}).
	 */
	static public final boolean USES_XML_VERSION_11 = false;
	
	static /*package*/ final String ATTRIBUTE_VALUESETCOLUMN_EMPTY = "empty";
	
	/**
	 * Different ways of representing composite columns (i.e. {@link ValueSetColumn}s)
	 */
	static public enum CompositeMode
	{
		/**
		 * Create one tag with name {@link RecordColumn#getName()} containing a single String value generated using {@link ValueSetColumn#toString(ValueSet)}.
		 */
		String,
		
		/**
		 * Create separate tags for each "leaf" column, names are constructed from parents' names and the leaf column's name, separated by {@link ValueSetColumn#QUALIFIED_NAME_SEPARATOR}.
		 */
		Flat,
		
		/**
		 * Create one tag with name {@link RecordColumn#getName()} containing child tags for the subcolumn.
		 */
		Nested;
		
		/**
		 * TODO use i18n/StringProvide
		 * 
		 * @see java.lang.Enum#toString()
		 */
		@Override
		public String toString()
		{
			switch(this)
			{
				case String:
					return "Single string";
				case Flat:
					return "Flat tags";
				case Nested:
					return "Grouped tags";
				default:
					return name();
			}
		}
		
	}
	
	static public CompositeMode DEFAULT_COMPOSITE_MODE = CompositeMode.Flat;
	
	// DYNAMIC ------------------------------------------------------
	private final CompositeMode compositeMode;
	private final XMLExportHelper helper = new XMLExportHelper();
	
	private int tabs = 0;
	private Record currentRecord = null;
	
	/**
	 * @param exportFolder
	 */
	public XMLRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_COMPOSITE_MODE);
	}
	
	/**
	 * @param exportFolder
	 * @param compositeMode
	 */
	public XMLRecordsExporter(File exportFolder, CompositeMode compositeMode)
	{
		if(exportFolder == null)
			throw new NullPointerException("Provide a non-null export folder!");
		this.exportFolder = exportFolder;
		this.compositeMode = compositeMode != null ? compositeMode : DEFAULT_COMPOSITE_MODE;
	}
	
	@Override
	protected void openWriter(String description, DateTime timestamp) throws IOException, FileStorageException
	{
		if(!FileHelpers.createDirectory(exportFolder))
			throw new FileStorageException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + "." + FILE_EXTENSION), Charsets.UTF_8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		writer.writeLine(XMLUtils.header(Charsets.UTF_8.name(), USES_XML_VERSION_11));
		writer.writeLine("<" + TAG_RECORDS_EXPORT + " " + ATTRIBUTE_EXPORTED_AT + "=\"" + ExportedAtFormatter.print(timestamp) + "\">");
	}
	
	@Override
	protected void closeWriter()
	{
		if(writer != null)
		{
			writer.writeLine("</" + TAG_RECORDS_EXPORT + ">");
			writer.dispose(); // also closes
			writer = null;
		}
	}
	
	@Override
	public ExportResult export(List<Record> records, String description)
	{
		if(records == null || records.isEmpty())
			return ExportResult.NothingToExport();
		
		// Sort records by Schema (& Model):
		Collections.sort(records, new Comparator<Record>()
		{
			Schema.Comparator schemaComparator = new Schema.Comparator();
			
			@Override
			public int compare(Record r1, Record r2)
			{
				return schemaComparator.compare(r1.getSchema(), r2.getSchema());
			}
		});
		
		// Export:
		List<Record> exported = new ArrayList<Record>();
		helper.reset();
		try
		{
			openWriter(description, DateTime.now());
			tabs = 1;
			currentRecord = null;
			for(Record r : records)
			{
				// Skip unexportable records unless force not to:
				if(!forceExportUnexportable && !r.getSchema().hasFlags(StorageClient.SCHEMA_FLAG_EXPORTABLE))
					continue;
				
				writer.openTransaction(); // output will be buffered
				try
				{
					//Open tag:
					writer.writeLine(StringUtils.addTabsFront(
						"<" + Record.TAG_RECORD + " " +
						Schema.ATTRIBUTE_SCHEMA_NAME + "=\"" + XMLUtils.escapeCharacters(r.getSchema().getName()) + "\" " +
						Schema.ATTRIBUTE_MODEL_ID + "=\"" + r.getSchema().getModelID() + "\" " +
						Schema.ATTRIBUTE_MODEL_SCHEMA_NUMBER + "=\"" + r.getSchema().getModelSchemaNumber() + "\"" +
						">", tabs));
				
					// Indent:
					tabs++;
				
					// Traverse columns:
					currentRecord = r;
					traverse(r.getSchema());
				
					// Unindent:
					tabs--;
				
					//Close tag:
					writer.writeLine(StringUtils.addTabsFront("</" + Record.TAG_RECORD + ">", tabs));
				}
				catch(Exception e)
				{
					writer.rollbackTransaction(); // !!!
					tabs = 1;
					writer.writeLine(XMLUtils.comment("Exception on exporting record: " + e.toString() + (e.getMessage() != null ? " [" + e.getMessage() + "]" : ""), tabs));
					throw e; //!!!
				}
				writer.commitTransaction(); // write out buffer
				exported.add(r);
				// TODO mark record as exported?
			}
			// Result...
			if(exported.size() == records.size())
				return ExportResult.Success(exported, exportFolder, Collections.singletonList(writer.getFile()));
			else
			{
				int unexportedCount = records.size() - exported.size();
				return ExportResult.PartialFailure(exported, exportFolder, Collections.singletonList(writer.getFile()), new UnexportableRecordsException(unexportedCount), unexportedCount);
			}
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			if(!exported.isEmpty())
				return ExportResult.PartialFailure(exported, exportFolder, Collections.singletonList(writer.getFile()), e, records.size() - exported.size());
			else
				return ExportResult.Failure(exportFolder, e, records.size());
		}
		finally
		{
			closeWriter();
		}
	}

	/**
	 * Here we ensure that parent tags are inserted for ValueSetColumns when in nester or flat mode.
	 * When the sub-ValueSet is empty (but non-{@null}) the tag gets the <code>empty="true"</code> attributes.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(ValueSetColumn<VS, CS> valueSetCol)
	{	
		if(compositeMode == CompositeMode.String)
			return; // this should never happen
		
		// Push on columnStack:
		super.enter(valueSetCol); // !!!
		
		// Write null value comment if subrecord is null:
		ValueSet<?> subRecord = valueSetCol.cast(getColumnPointer().retrieveValue(currentRecord));
		if(subRecord == null)
		{	// Subrecord is null: write null comment
			writer.writeLine(StringUtils.addTabsFront(getNullColumnComment(valueSetCol.getName()), tabs));
		}
		else if(compositeMode == CompositeMode.Nested)
		{	// If in nested tags mode and subrecord is not null, open parent tag (with empty attribute if necessary):
			writer.writeLine(StringUtils.addTabsFront(
				"<" + valueSetCol.getName() + (subRecord.isEmpty() ? " " + ATTRIBUTE_VALUESETCOLUMN_EMPTY + "=\"" + Boolean.TRUE.toString() + "\"" : "") + ">",
				tabs));
			tabs++;
		}
		else if(/*compositeMode == CompositeMode.Flat && */subRecord.isEmpty())
		{	// if in flat tags mode and subrecord is not null but empty(!), write collapsed parent tag
			//	(with empty attribute, even though the parser doesn't strictly need that in this case): 
			writer.writeLine(StringUtils.addTabsFront(
				"<" + valueSetCol.getName() + " " + ATTRIBUTE_VALUESETCOLUMN_EMPTY + "=\"" + Boolean.TRUE.toString() + "\"/>",
				tabs));
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser#leave(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void leave(ValueSetColumn<VS, CS> valueSetCol)
	{
		if(compositeMode == CompositeMode.String)
			return; // this should never happen

		// If in nested tags mode and subrecord is not null, close parent tag:
		if(compositeMode == CompositeMode.Nested && getColumnPointer().retrieveValue(currentRecord) != null)
		{	// Close parent tag:
			tabs--;
			writer.writeLine(StringUtils.addTabsFront("</" + valueSetCol.getName() + ">", tabs));
		}
		
		// Pop columnStack:
		super.leave(valueSetCol);
	}
	
	@Override
	public void visit(ColumnPointer<?> leafColumnPointer)
	{
		ValueSet<?> valueSet = leafColumnPointer.getValueSet(currentRecord, false);
		
		// If in nested or flat tags mode and subrecord is null: return
		if(valueSet == null && compositeMode != CompositeMode.String)
			return;
		
		// Write column value or null value comment:
		Column<?> leafColumn = leafColumnPointer.getColumn();
		String columnName = (compositeMode == CompositeMode.Flat ? leafColumnPointer.getQualifiedColumnName() : leafColumn.getName());
		String tagContent = helper.getValueString(leafColumn, valueSet, null);
		if(tagContent == null)
			writer.writeLine(StringUtils.addTabsFront(getNullColumnComment(columnName), tabs));
		else if(tagContent.isEmpty())
			writer.writeLine(StringUtils.addTabsFront("<" + columnName + "/>", tabs)); // collapsed tag
		else
			writer.writeLine(StringUtils.addTabsFront("<" + columnName + ">" + tagContent + "</" + columnName + ">", tabs));
	}
	
	private String getNullColumnComment(String columnName)
	{
		return XMLUtils.comment(columnName + " is null");
	}
	
	@Override
	public boolean splitLocationTraversal()
	{
		return compositeMode != CompositeMode.String;
	}

	@Override
	public boolean splitOrientationTraversal()
	{
		return compositeMode != CompositeMode.String;
	}

	@Override
	public boolean splitForeignKeyTraversal()
	{
		return compositeMode != CompositeMode.String;
	}

	@Override
	public boolean skipNonBinarySerialisedLocationSubColumns()
	{
		return false;
	}

	@Override
	public boolean skipNonBinarySerialisedOrientationSubColumns()
	{
		return false;
	}

	@Override
	public boolean includeVirtualColumns()
	{
		return true;
	}
	
	/**
	 * Helper class which creates String representations, escaped as necessary, of column values.
	 * 
	 * For {@link ListLikeColumn}s (i.e. {@link StringColumn} and {@link ListColumn}s) we don't use the
	 * Column's own serialisation delimiters because the XML format allows us to preserve the difference
	 * between {@code null} and an empty list/String value:
	 * 	- <!-- columnName is null -->					: null value
	 *  - <ColumnName></ColumnName>	or <ColumnName/>	: empty list/String value
	 * 
	 * @see ExportHelper
	 * 
	 * @author mstevens
	 */
	/*package*/ class XMLExportHelper extends ExportHelper
	{

		/**
		 * Performs XML 1.0/1.1 escaping (there is no quoting needed).
		 * 
		 * @param valueString String to escape
		 * @param force ignore here (we always escape, forcing doesn't make a difference)
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExportHelper#escapeAndQuote(java.lang.String, boolean)
		 */
		@Override
		protected String escapeAndQuote(String valueString, boolean force)
		{
			return USES_XML_VERSION_11 ? StringEscapeUtils.escapeXml11(valueString) : StringEscapeUtils.escapeXml10(valueString);
		}
		
	}
	
}
