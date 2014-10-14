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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileWriter;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.UnicodeHelpers;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.SimpleSchemaTraverser;


/**
 * Class to export {@link Record}s to XML files, which can be re-imported by {@link XMLRecordsImporter}.
 * 
 * Follows the CSV specification outlined in RFC 4180 (i.e. with regardis to escaping/quoting), except for
 * the choice between different separators (tab & semicolon in addition to comma).
 * The CSV will get a header line with the column names separated by the separator, with this postfix: separator+"modelID="+...+separator+"modelSchemaNumber="+...+separator
 * 
 * @author mstevens
 * @see <a href="http://www.ietf.org/rfc/rfc4180.txt">http://www.ietf.org/rfc/rfc4180.txt</a>
 */
public class CSVRecordsExporter extends SimpleSchemaTraverser implements Exporter
{

	// STATICS------------------------------------------------------- 
	static private final char DOUBLE_QUOTE = '"';
	
	static public enum Separator
	{
		COMMA,
		SEMICOLON,
		TAB;
		
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
	
	static public final Separator DEFAULT_SEPARATOR = Separator.COMMA;
	
	// DYNAMICS------------------------------------------------------
	private File exportFolder;
	private Separator separator;
	
	private FileWriter writer = null;
	private List<ColumnPointer> columnPointers;
	
	public CSVRecordsExporter(File exportFolder)
	{
		this(exportFolder, DEFAULT_SEPARATOR);
	}
	
	public CSVRecordsExporter(File exportFolder, Separator separator)
	{
		if(!FileHelpers.createFolder(exportFolder))
			throw new IllegalArgumentException("Export folder (" + exportFolder + ") does not exist and could not be created!");
		this.exportFolder = exportFolder;
		this.separator = separator;
		this.columnPointers = new ArrayList<ColumnPointer>();
	}
	
	private void openWriter(String description, DateTime timestamp) throws Exception
	{
		writer = new FileWriter(exportFolder + File.separator + FileHelpers.makeValidFileName("Records_" + description + "_" + TimeUtils.getTimestampForFileName(timestamp) + ".csv"), UnicodeHelpers.UTF8);
		writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);	
	}
	
	private void deleteFile()
	{
		if(writer != null)
			writer.delete();
		closeWriter();
	}
	
	private void closeWriter()
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
		
		// Group records by schema:
		Map<Schema, List<Record>> recordsBySchema = new HashMap<Schema, List<Record>>();
		for(Record r : records)
		{
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
		List<File> csvFiles = new ArrayList<File>();
		for(Map.Entry<Schema, List<Record>> entry : recordsBySchema.entrySet())
		{
			try
			{
				Schema schema =  entry.getKey();
				openWriter(description + "_" + schema.getName(), timestamp);

				// Construct column list:
				columnPointers.clear();
				traverse(schema);
				
				// Write header:
				writer.openTransaction(); // output will be buffered
				try
				{
					// Column names (separated by the separator):
					for(ColumnPointer cp : columnPointers)
						writer.write((!writer.isTransactionBufferEmpty() ? separator.getSeparatorChar() : "") + cp.getQualifiedColumnName());
					// Postfix: separator+"modelID="+...+separator+"modelSchemaNumber="+...+separator
					writer.write(	separator.getSeparatorChar() + Schema.ATTRIBUTE_MODEL_ID + "=" + schema.getModelID() +
									separator.getSeparatorChar() + Schema.ATTRIBUTE_MODEL_SCHEMA_NUMBER + "=" + schema.getModelSchemaNumber() +
									separator.getSeparatorChar());
					writer.write('\n');
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
					if(r.getSchema().isInternal())
						continue; // we do not export records of internal schemata
					writer.openTransaction(); // output will be buffered
					try
					{	
						for(ColumnPointer cp : columnPointers)
						{
							if(!writer.isTransactionBufferEmpty())
								writer.write(separator.getSeparatorChar());
							Column<?> col = cp.getColumn();
							Record rec = cp.getRecord(r, false);
							if(rec != null && col.isValueSet(rec)) // do write nothing when the value is not set (i.e. null is represented by an empty String)
							{
								/* Get String representing the column value...
								 * 	If the column type is String (meaning it is a StringColumn or a VirtualColumn with a StringColumn as its target),
								 * 	then we use the raw String value returned by StringColumn#retrieveValue() instead of the singe-quoted String returned
								 * 	by StringColumn#retrieveValueAsString(). */
								String valueString = (col.getType() == String.class ? (String) col.retrieveValue(rec) : col.retrieveValueAsString(rec));
								/* Write value and escape/quote if necessary or required...
								 * 	We force (double) quotes on StringColumns in order to maintain ability to differentiate null and empty String.
								 * 	We don't need to do this on VirtualColumns with target type String because their values will get quoted when needed
								 * 	(i.e. due to occurrence of double quote, new line or separator) but they don't need to maintain the difference between
								 * 	null and empty String since virtual columns would be ignored if we ever implement a CSVRecordsImporter class. */	
								writer.write(escapeAndQuote(valueString, col instanceof StringColumn));
							}
						}
						writer.write('\n');
					}
					catch(Exception e)
					{
						writer.rollbackTransaction(); // !!!
						throw e;
					}
					writer.commitTransaction(); // write out buffer
					exported.add(r);
					// TODO mark record as exported?
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
		}
		// Success:
		return ExportResult.Success(exported, exportFolder, csvFiles);
	}
	
	private String escapeAndQuote(String valueString, boolean forceQuotes)
	{
		boolean needsQuotes = forceQuotes;
		StringBuilder bldr = new StringBuilder(valueString.length());
		for(char c : valueString.toCharArray())
		{
			if(c == DOUBLE_QUOTE)
				bldr.append(DOUBLE_QUOTE); // escape double quote occurrences by doubling them
			if(c == DOUBLE_QUOTE || c == '\r' || c == '\n' || c == separator.getSeparatorChar())
				needsQuotes = true;
			bldr.append(c);
		}			
		return (needsQuotes ? DOUBLE_QUOTE : "") + bldr.toString() + (needsQuotes ? DOUBLE_QUOTE : "");
	}
	
	@Override
	public void visit(ColumnPointer leafColumnPointer)
	{
		columnPointers.add(leafColumnPointer);
	}
	
	@Override
	public boolean allowLocationSelfTraversal()
	{
		return true;
	}

	@Override
	public boolean allowOrientationSelfTraversal()
	{
		return true;
	}

	@Override
	public boolean allowForeignKeySelfTraversal()
	{
		return true;
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
	
}
