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

package uk.ac.ucl.excites.sapelli.storage.eximport.csv;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.io.Charsets;
import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.text.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.SimpleExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExportHelper;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
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
 * Follows the CSV specification outlined in RFC 4180 (i.e. with regards to escaping/quoting),
 * except for the choice between different separators (tab & semicolon in addition to comma).
 * 
 * Note 1:
 * 	The CSV will get a header line with the column names separated by the separator,
 * 	followed by this postfix (assuming separator is <code>,</code>):
 * 		<code>,modelID=XXXXXXXXXXXXXXXX,modelSchemaNumber=YY,schemaName="abcdef",exportedAt=TTTTTTTTTTTTTTT,</code>
 * 	The trailing separator allows parsing code to detect the separator by simply reading
 * 	the last char of the first line.
 * 
 * Note 2:
 *	We want to maintain the difference between a ValueSetColumn set to {@code null} and one
 *	which contains a sub-ValueSet with all {@code null} values. Because ValueSetColumns are
 *	split up in the CSV we need a trick to do this. The solution is to insert a Boolean-like
 *	column with the name of the ValueSetColumn before its subcolumns. When the ValueSetColumn
 *	contains a non-{@code null} value this inserted column will be set to {@code true}, if not
 *	it will remain empty (i.e. representing a {@code null} value).
 * 
 * @author mstevens
 * 
 * @see <a href="http://www.ietf.org/rfc/rfc4180.txt">http://www.ietf.org/rfc/rfc4180.txt</a>
 */
public class CSVRecordsExporter extends SimpleExporter
{

	// STATICS-------------------------------------------------------
	static public final String FILE_EXTENSION = "csv";
	
	static public final char DOUBLE_QUOTE = '"';
	static public final char LINE_ENDING = '\n';
	
	static public enum Separator
	{
		COMMA,
		SEMICOLON,
		TAB;
		
		static public Separator getSeparator(char separatorChar)
		{
			switch(separatorChar)
			{
				case ',':
					return COMMA;
				case ';':
					return SEMICOLON;
				case '\t':
					return TAB;
				default:
					throw new IllegalStateException("Invalid CSV sepator: " + separatorChar);
			}
		}
		
		/**
		 * TODO use i18n/StringProvide
		 * 
		 * @see java.lang.Enum#toString()
		 */
		@Override
		public String toString()
		{
			return name().toLowerCase(Locale.getDefault());
		}
		
		public char getSeparatorChar()
		{
			switch(this)
			{
				case COMMA:
					return ',';
				case SEMICOLON:
					return ';';
				case TAB:
					return '\t';
				default:
					throw new IllegalStateException("Unknown CSV sepator: " + name());
			}
		}
		
	}
	
	static /*package*/ List<ColumnPointer<?>> GetColumnPointers(Schema schema)
	{
		return new CSVRecordsExporter(null, DEFAULT_SEPARATOR, false).getColumnPointers(schema);
	}
	
	static public final Separator DEFAULT_SEPARATOR = Separator.COMMA;
	
	static /*package*/ final String NON_NULL_SUB_VALUESET = Boolean.TRUE.toString();
	
	// DYNAMICS------------------------------------------------------
	private final Separator separator;
	private final char[] avoidChars;
	private final List<ColumnPointer<?>> columnPointers = new ArrayList<ColumnPointer<?>>();
	private final CSVExportHelper valueStringProvider = new CSVExportHelper();
	
	/**
	 * @param exportFolder
	 */
	public CSVRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_SEPARATOR);
	}
	
	/**
	 * @param exportFolder
	 * @param separator
	 */
	public CSVRecordsExporter(File exportFolder, Separator separator)
	{
		this(exportFolder, separator, true);
	}
	
	/**
	 * @param exportFolder
	 * @param separator
	 * @param checkFolder
	 */
	private CSVRecordsExporter(File exportFolder, Separator separator, boolean checkFolder)
	{
		if(checkFolder && exportFolder == null)
			throw new NullPointerException("Provide a non-null export folder!");
		this.exportFolder = exportFolder;
		this.separator = separator != null ? separator : DEFAULT_SEPARATOR;
		this.avoidChars = new char[] { separator.getSeparatorChar(), '\n', '\r' }; // !!! 
	}
	
	@Override
	protected void openWriter(String description, DateTime timestamp) throws IOException, FileStorageException
	{
		if(!FileHelpers.createDirectory(exportFolder))
			throw new FileStorageException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + "." + FILE_EXTENSION), Charsets.UTF_8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);	
	}
	
	private void deleteFile()
	{
		if(writer != null)
			writer.delete();
		closeWriter();
	}
	
	@Override
	protected void closeWriter()
	{
		if(writer != null)
		{
			writer.dispose(); // also closes
			writer = null;
		}
	}
	
	/**
	 * @param records all assumed to be of the same top-level schema
	 * @param description
	 * @return
	 * @throws Exception
	 */
	@Override
	public ExportResult export(List<Record> records, String description)
	{
		if(records == null || records.isEmpty())
			return ExportResult.NothingToExport();
		
		// Timestamp for filenames:
		DateTime timestamp = DateTime.now();
		
		// Group records by schema & filter out records of unexportable schemata:
		Map<Schema, List<Record>> recordsBySchema = new HashMap<Schema, List<Record>>();
		for(Record r : records)
		{
			// Skip unexportable records unless force not to:
			if(!forceExportUnexportable && !r.getSchema().hasFlags(StorageClient.SCHEMA_FLAG_EXPORTABLE))
				continue;
			
			List<Record> recordsForSchema;
			if(recordsBySchema.containsKey(r.getSchema()))
				recordsForSchema = recordsBySchema.get(r.getSchema());
			else
			{
				recordsForSchema = new ArrayList<Record>();
				recordsBySchema.put(r.getSchema(), recordsForSchema);
			}
			recordsForSchema.add(r);
		}
		
		// Export each group to a separate CSV file:
		List<Record> exported = new ArrayList<Record>();
		List<Record> exportedForSchema = new ArrayList<Record>();
		List<File> csvFiles = new ArrayList<File>();
		valueStringProvider.reset();
		for(Map.Entry<Schema, List<Record>> entry : recordsBySchema.entrySet())
		{
			try
			{
				Schema schema =  entry.getKey();
				openWriter(description + "_" + schema.getName(), timestamp);

				// Construct column list:
				getColumnPointers(schema);
				
				// Write header:
				writer.openTransaction(); // output will be buffered
				try
				{
					// Column names (separated by the separator):
					for(ColumnPointer<?> cp : columnPointers)
						writer.write((!writer.isTransactionBufferEmpty() ? separator.getSeparatorChar() : "") + cp.getQualifiedColumnName());
					// Postfix (assuming separator is ,): ,modelID=XXXXXXXXXXXXXXXX,modelSchemaNumber=YY,schemaName="abcdef",
					writer.write(	separator.getSeparatorChar() + Schema.ATTRIBUTE_MODEL_ID + "=" + schema.getModelID() +
									separator.getSeparatorChar() + Schema.ATTRIBUTE_MODEL_SCHEMA_NUMBER + "=" + schema.getModelSchemaNumber() +
									separator.getSeparatorChar() + Schema.ATTRIBUTE_SCHEMA_NAME + "=" + escapeAndQuote(schema.getName(), true) +
									separator.getSeparatorChar() + ATTRIBUTE_EXPORTED_AT + "=" + ExportedAtFormatter.print(timestamp) +
									separator.getSeparatorChar());
					writer.write(LINE_ENDING);
				}
				catch(Exception e)
				{
					writer.rollbackTransaction(); // !!!
					throw e;
				}
				writer.commitTransaction(); // write out buffer
				
				// Write records:
				for(Record r : entry.getValue())
				{
					writer.openTransaction(); // output will be buffered
					try
					{
						boolean first = true;
						for(ColumnPointer<?> cp : columnPointers)
						{
							if(!first)
								writer.write(separator.getSeparatorChar());
							else
								first = false;							
							writer.write(valueStringProvider.getValueString(cp.getColumn(), cp.getValueSet(r, false), ""));
							// will write nothing (i.e. "") when the value is not set (i.e. null value is represented by an empty String)
						}
						writer.write(LINE_ENDING);
					}
					catch(Exception e)
					{
						writer.rollbackTransaction(); // !!!
						throw e;
					}
					writer.commitTransaction(); // write out buffer
					exportedForSchema.add(r);
				}
				csvFiles.add(writer.getFile());
				closeWriter();
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				deleteFile();
				if(!exported.isEmpty())
					return ExportResult.PartialFailure(exported, exportFolder, csvFiles, e, records.size() - exported.size());
				else
					return ExportResult.Failure(exportFolder, e, records.size());
			}
			
			// Move records exported for current schema to overall exported list:
			exported.addAll(exportedForSchema);
			exportedForSchema.clear();
			// TODO mark record as exported?
		}
		// Result...
		if(exported.size() == records.size())
			return ExportResult.Success(exported, exportFolder, csvFiles);
		else
		{
			int unexportedCount = records.size() - exported.size();
			return ExportResult.PartialFailure(exported, exportFolder, csvFiles, new UnexportableRecordsException(unexportedCount), unexportedCount);
		}
	}
	
	protected List<ColumnPointer<?>> getColumnPointers(Schema schema)
	{
		columnPointers.clear();
		traverse(schema);
		return columnPointers;
	}
	
	/*package*/ String escapeAndQuote(String valueString, boolean forceQuotes)
	{
		return StringUtils.escapeByDoublingAndWrapping(valueString, avoidChars, DOUBLE_QUOTE, forceQuotes);
	}
	
	@Override
	public void visit(ColumnPointer<?> leafColumnPointer)
	{
		columnPointers.add(leafColumnPointer);
	}
	
	/**
	 * This relates to Note 2 in {@link CSVRecordsExporter} javadoc.
	 * 
	 * @see CSVExportHelper#getValueSetString(ValueSetColumn)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(final ValueSetColumn<VS, CS> valueSetCol)
	{
		super.enter(valueSetCol); // !!!
		
		// Add CSV column for the ValueSetColumn itself:
		//	(for differentiating null sub-ValueSet from non-null sub-ValueSets with all null values)
		columnPointers.add(new ColumnPointer<Column<?>>(valueSetCol)); // column will be treated in ColumnValueStringProvider#getValueSetString(ValueSetColumn)
	}
	
	@Override
	public boolean splitLocationTraversal()
	{
		return true; // split up location in subcolumns
	}

	@Override
	public boolean splitOrientationTraversal()
	{
		return true; // split up orientation in subcolumns
	}

	@Override
	public boolean splitForeignKeyTraversal()
	{
		return true; // split up foreign keys in subcolumns
	}
	
	/**
	 * Helper class which creates String representations, escaped and quoted as necessary, of column values.
	 * 
	 * The string representations of {@link ListLikeColumn}s (i.e. {@link StringColumn} and {@link ListColumn}s)
	 * are always quoted (and thus also escaped), and we don't use the Column's own serialisation delimiters
	 * because the quoting allows us to preserve the difference between {@code null} and an empty list/String value:
	 * 	- SEPARATOR+SEPARATOR --> null value
	 *  - SEPARATOR+"+"+SEPARATOR --> empty value
	 *  
	 * The class also provides the String ({@link CSVRecordsExporter#NON_NULL_SUB_VALUESET}) representing non-null sub-ValueSets.
	 * 
	 * @see CSVRecordsExporter
	 * @see ExportHelper
	 * 
	 * @author mstevens
	 */
	/*package*/ class CSVExportHelper extends ExportHelper
	{

		@Override
		protected String escapeAndQuote(String valueString, boolean force)
		{
			return CSVRecordsExporter.this.escapeAndQuote(valueString, force);
		}

		/**
		 * This relates to Note 2 in {@link CSVRecordsExporter} javadoc.
		 * 
		 * @see CSVRecordsExporter#enter(ValueSetColumn)
		 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ExportHelper#getSubValueSetString(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn, uk.ac.ucl.excites.sapelli.storage.model.ValueSet)
		 */
		@Override
		protected <VS extends ValueSet<CS>, CS extends ColumnSet> String getSubValueSetString(ValueSetColumn<VS, CS> valueSetCol, VS subValueSet)
		{
			return NON_NULL_SUB_VALUESET;
		}
		
	}
	
}
