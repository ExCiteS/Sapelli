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

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.text.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.SimpleExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExportColumnValueStringProvider;
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

	// STATICS-------------------------------------------------------
	static public final String TAG_RECORDS_EXPORT = "RecordsExport";
	static public final boolean USES_XML_VERSION_11 = false; // whether to use XML v1.0 (false) or XML v1.1 (true)

	/**
	 * Different ways of representing composite columns (i.e. {@link ValueSetColumn}s)
	 */
	static public enum CompositeMode
	{
		/**
		 * Create one tag with name {@link RecordColumn#getName()} containing a single String value generated using {@link ValueSetColumn#toString(Record)}.
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
	
	// DYNAMICS------------------------------------------------------
	private CompositeMode compositeMode;
	private ColumnValueStringProvider valueStringProvider;
	
	private int tabs = 0;
	
	private Record currentRecord;
	
	public XMLRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_COMPOSITE_MODE);
	}
	
	public XMLRecordsExporter(File exportFolder, CompositeMode compositeMode)
	{
		if(!FileHelpers.createDirectory(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.compositeMode = compositeMode;
		this.currentRecord = null;
	}
	
	@Override
	protected void openWriter(String description, DateTime timestamp) throws IOException
	{
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + ".xml"), Charsets.UTF_8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		writer.writeLine(XMLUtils.header(Charsets.UTF_8.name(), USES_XML_VERSION_11));
		writer.writeLine("<" + TAG_RECORDS_EXPORT + " exportedAt=\"" + TimeUtils.getISOTimestamp(timestamp, false) + "\">");
		//TODO add attributes: comment, device(?)
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
		valueStringProvider = new ColumnValueStringProvider();
		try
		{
			openWriter(description, DateTime.now());
			tabs = 1;
			for(Record r : records)
			{
				// Skip unexportable records unless force not to:
				if(!forceExportUnexportable && !r.getSchema().hasFlags(StorageClient.SCHEMA_FLAG_EXPORTABLE))
					continue;
				
				writer.openTransaction(); // output will be buffered
				try
				{				
					//Open tag:
					writer.writeLine(StringUtils.addTabsFront("<" + Record.TAG_RECORD + " " +
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(ValueSetColumn<VS, CS> valueSetCol)
	{
		if(compositeMode == CompositeMode.String)
			return; // this should never happen
		
		// Push on columnStack:
		super.enter(valueSetCol);
		
		// Write null value comment if subrecord is null:
		if(getColumnPointer().retrieveValue(currentRecord) == null)
			writer.writeLine(StringUtils.addTabsFront(getNullRecordComment(valueSetCol.getName()), tabs));
		else if(compositeMode == CompositeMode.Nested)
		{	// If in nested tags mode and subrecord is not null, open parent tag:
			writer.writeLine(StringUtils.addTabsFront("<" + valueSetCol.getName() + ">", tabs));
			tabs++;
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
		{
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
		if(valueSet != null && leafColumn.isValuePresent(valueSet))
			writer.writeLine(StringUtils.addTabsFront("<" + columnName + ">" + valueStringProvider.toString(leafColumn, valueSet) + "</" + columnName + ">", tabs));
		else
			writer.writeLine(StringUtils.addTabsFront(getNullRecordComment(columnName), tabs));
	}
	
	private String getNullRecordComment(String columnName)
	{
		return XMLUtils.comment(columnName + " is null");
	}
	
	@Override
	public boolean allowLocationSelfTraversal()
	{
		return compositeMode != CompositeMode.String;
	}

	@Override
	public boolean allowOrientationSelfTraversal()
	{
		return compositeMode != CompositeMode.String;
	}

	@Override
	public boolean allowForeignKeySelfTraversal()
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
	 * 	- <!-- columnName is null -->	: null value
	 *  - <ColumnName></ColumnName>		: empty list/String value
	 * 
	 * @see ExportColumnValueStringProvider
	 * 
	 * @author mstevens
	 */
	private class ColumnValueStringProvider extends ExportColumnValueStringProvider
	{

		@Override
		protected String escapeAndQuote(String valueString, boolean force)
		{
			return USES_XML_VERSION_11 ? StringEscapeUtils.escapeXml11(valueString) : StringEscapeUtils.escapeXml10(valueString); // always escape (forcing doesn't make a difference)
		}
		
	}
	
}
