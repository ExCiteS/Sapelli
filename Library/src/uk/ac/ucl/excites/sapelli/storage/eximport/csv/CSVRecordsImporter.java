/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.apache.commons.compress.utils.Charsets;

import uk.ac.ucl.excites.sapelli.shared.io.text.UnicodeBOMInputStream;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper.WarningKeeperImpl;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.Importer;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ImportHelper;
import uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ImportHelper.CustomValueParser;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * An {@link Importer} class which imports {@link Record}s from CSV files,
 * typically/ideally as exported by the {@link CSVRecordsExporter}. 
 * 
 * @author mstevens
 */
public class CSVRecordsImporter extends WarningKeeperImpl implements Importer
{
	
	// STATIC -------------------------------------------------------
	static private final char DOUBLE_QUOTE = '"';
	
	// DYNAMIC ------------------------------------------------------
	protected final StorageClient client;

	protected final CSVImportHelper helper;

	protected Exception headerError;
	protected Separator separator;
	protected TimeStamp exportedAt;
	protected Schema schema;
	protected List<ColumnPointer<?>> columnPointers;

	protected List<Record> records;
	protected int rowCount;
	
	public CSVRecordsImporter(StorageClient client)
	{
		super();
		this.client = client;
		this.helper = new CSVImportHelper();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.xml.Importer#importFrom(java.io.File)
	 */
	@Override
	public List<Record> importFrom(File csvFile) throws Exception
	{
		return importFrom(csvFile, null);
	}
	
	/**
	 * @param csvFile
	 * @param fallbackSchema may be null
	 * @return
	 * @throws Exception
	 * @see {@link #importFrom(File)}
	 */
	public List<Record> importFrom(File csvFile, Schema fallbackSchema) throws Exception
	{
		// Allocate a new records list:
		records = new ArrayList<Record>(); // (don't reuse old one as this object is returned)

		// (Re)initialise data structures:
		headerError = null;
		rowCount = 0;
		separator = null;
		exportedAt = null;
		schema = fallbackSchema;
		columnPointers = fallbackSchema == null ? null : CSVRecordsExporter.GetColumnPointers(fallbackSchema);
		
		// Parse the file:
		parse(csvFile);
		
		// Return parsed records:
		return records;
	}

	private void parse(File csvFile) throws Exception
	{
		try(UnicodeBOMInputStream input = new UnicodeBOMInputStream(new FileInputStream(csvFile));
			BufferedReader reader = new BufferedReader(input.getReader(Charsets.UTF_8)))
		{
			int r;
			int dqCount = 0;
			StringBuilder rowBldr = new StringBuilder();
			while((r = reader.read()) != -1)
			{
				char c = (char) r;
				if(c == DOUBLE_QUOTE)
				{	// count the number of double quotes we've passed
					dqCount++;
					// Append char:
					rowBldr.append(c); // !!!
				}
				else if(c == CSVRecordsExporter.LINE_ENDING && dqCount % 2 == 0)
				{	// if dqCount is even this means we are not inside a quoted value and this is an actual row line break
					String row = rowBldr.toString();
					rowBldr.setLength(0); // reset line builder!
					// Parse row:
					parseRow(row);
				}
				else
				{	
					// Append char:
					rowBldr.append(c); // !!!
				}
			}
		}
		catch(Exception e) // only for unrecoverable errors
		{
			throw new Exception("Error upon parsing CSV file (" + (csvFile != null ? csvFile.getName() : "null") + ")!", e); 
		}
	}
	
	private void parseRow(String row) throws Exception
	{
		// Increase row counter:
		rowCount++; // !!!
		
		if(rowCount == 1)
		{	// Header row...
			try
			{
				parseHeaderRow(row);
			}
			catch(Exception e)
			{
				if(schema != null && columnPointers != null)
				{	// we were given a fallback schema, so perhaps this was a record row (and not a header row)...
					headerError = e;
					rowCount--;
					parseRow(row); // try parsing as record row
				}
				else
					throw e;
			}
		}
		else
		{	// Record row...
			if(schema == null || columnPointers == null)
				throw new Exception("Cannot parse record rows if no schema is known.");
			Record parsedRecord = null;
			try
			{
				parsedRecord = parseRecordRow(row); 
			}
			catch(Exception e)
			{
				if(rowCount == 1 && headerError != null)
					// we already had trouble when parsing this as a header row, throw that exception instead of a new one:
					throw headerError;
				addWarning("Error on parsing record (line #" + rowCount + "): " + ExceptionHelpers.getMessageAndCause(e));
			}
			if(parsedRecord != null)
			{
				// Set missing required values to default (recursively):
				parsedRecord.resetEmptyColumns(true, true);
				
				// Recursive "filledness" check:
				if(!parsedRecord.isFilled(true))
					addWarning("Imported record (line #" + rowCount + ") is incomplete: " + parsedRecord.toString(false));
				
				// Add parsed record:
				records.add(parsedRecord);
			}
		}
	}
	
	private void parseHeaderRow(String row) throws Exception
	{		
		// Check row length:
		if(row.isEmpty())
			throw new IllegalArgumentException("Header row cannot be null");
		
		// Get separator by reading last char of the header:
		try
		{
			separator = Separator.getSeparator(row.charAt(row.length() - 1));
		}
		catch(IllegalArgumentException iae)
		{
			separator = CSVRecordsExporter.DEFAULT_SEPARATOR;
			addWarning("Header row does no contain separator hint, trying to parse file using default separator (" + separator.toString() + ").");
		}
		
		// Split header row:
		List<String> headers = splitRow(row);
		
		// Parse attribute headers:
		Long modelID = null;
		Integer modelSchemaNo = null;
		String schemaName = null;
		try
		{
			ListIterator<String> headerIter = headers.listIterator(headers.size());
			String attributeHeader;
			int equalsPos;
			//	Iterate over headers back to front until we hit one without '=':
			while(headerIter.hasPrevious() && (equalsPos = (attributeHeader = headerIter.previous()).indexOf('=')) != -1)
			{
				switch(attributeHeader.substring(0, equalsPos))
				{
					case Schema.ATTRIBUTE_MODEL_ID :
						modelID = Long.valueOf(attributeHeader.substring(equalsPos + 1));
						break;
					case Schema.ATTRIBUTE_MODEL_SCHEMA_NUMBER :
						modelSchemaNo = Integer.valueOf(attributeHeader.substring(equalsPos + 1));
						break;
					case Schema.ATTRIBUTE_SCHEMA_NAME :
						schemaName = deescapeAndUnquote(attributeHeader.substring(equalsPos + 1));
						break;
					case Exporter.ATTRIBUTE_EXPORTED_AT :
						try
						{
							exportedAt = new TimeStamp(Exporter.ExportedAtFormatter.withOffsetParsed().parseDateTime(attributeHeader.substring(equalsPos + 1)));
						}
						catch(Exception e)
						{
							addWarning("Error upon parsing exportedAt time: " + attributeHeader.substring(equalsPos + 1));
						}
						break;
					default :
						addWarning("Ignored unrecognised attribute header: " + attributeHeader);
				}
				// Remove attribute header:
				headerIter.remove();
			}
		}
		catch(Exception e)
		{
			// don't throw here
			client.logWarning("Error upon parsing CSV attribute header: " + ExceptionHelpers.getMessageAndCause(e));
		}
		
		// Get schema:
		if(modelID != null && modelSchemaNo != null)
		{
			Schema headerSchema = null;
			try
			{
				headerSchema = client.getSchema(modelID, modelSchemaNo, schemaName);
			}
			catch(Exception e)
			{
				if(schema != null)
					addWarning("Could not find schema: " + e.getMessage() + ". Using fallback schema (" + schema.toString() + ").");
				else
					throw e;
			}
			if(schema != null && !headerSchema.equals(schema))
				addWarning("CSV schema (" + headerSchema.toString() + ") is different from given fallback schema (" + schema.toString() + ").");
			schema = headerSchema; // !!!
		}
		else
		{
			String error = "No (readable) model/schema information in header!";
			if(schema != null)
				addWarning(error + " Using fallback schema (" + schema.toString() + ").");
			else
				throw new Exception("Could not find model/schema information in header!");
		}
		
		if(schema != null)
		{
			// Parse column headers:
			List<ColumnPointer<?>> headerColumnPointers = new ArrayList<ColumnPointer<?>>(headers.size());
			for(String columnHeader : headers) // (the remaining headers should all be qualified column names)
				headerColumnPointers.add(ColumnPointer.ByName(schema, columnHeader, false, false));
			// if we get her nothing went wrong above:
			this.columnPointers = headerColumnPointers;
		}
	}
	
	private Record parseRecordRow(String line) throws Exception
	{
		// Create new, initialised record:
		Record record = helper.initialise(schema.createRecord());
		
		// Get list of values to parse:
		List<String> valueStrings = splitRow(line);
				
		// Check number of columns/values:
		if(columnPointers.size() != valueStrings.size()) // String is expected to contain as many values as separators
			throw new Exception("CSV record row has unexpected number of values (expected: " + columnPointers.size() + "; found: " + valueStrings.size() + ")!");
		
		// Get each valueString, de-espace & unquote it and parse it using the corresponding column:
		Iterator<String> valueStringIter = valueStrings.iterator();
		Iterator<ColumnPointer<?>> cpIter = columnPointers.iterator();
		while(valueStringIter.hasNext() && cpIter.hasNext())
		{
			String valueString = valueStringIter.next();
			ColumnPointer<?> currentCP = cpIter.next();
			
			// Check if valueString is empty:
			if(valueString.isEmpty())
				continue; // empty valueStrings (prior to deescaping/unquoting) in the CSV always represent null values!
			
			// Parse & store:
			try
			{
				// Get the pointed-at column:
				Column<?> column = currentCP.getColumn();
				
				// We always ignore virtual column values as they never store their own value
				if(column instanceof VirtualColumn)
					continue; // skip!
				
				// Get/create&initialise (sub)ValueSet/Record:
				ValueSet<?> valueSet = currentCP.getValueSet(record, true, helper);

				// Use helper to parse & store value:
				helper.parseAndStoreValue(column, deescapeAndUnquote(valueString), valueSet);
			}
			catch(Exception e)
			{
				addWarning("Error upon parsing value (" + valueString + ") for column " + currentCP.getQualifiedColumnName() + ": " + ExceptionHelpers.getMessageAndCause(e));
			}
		}
		
		// Done!
		return record;
	}
	
	private List<String> splitRow(String line)
	{
		List<String> parts = new ArrayList<String>();
		
		// Add trailing separator if it isn't there (this simplifies the code below):
		if(line.isEmpty() || line.charAt(line.length() - 1) != separator.getSeparatorChar())
			line += separator.getSeparatorChar();
		
		// Find the separator positions and get each value/header String:		
		int dqCount = 0;
		int startPointer = 0;
		for(int i = 0; i < line.length(); i++)
		{
			char c = line.charAt(i);
			if(c == DOUBLE_QUOTE)
				// count the number of double quotes we've passed
				dqCount++;
			else if(c == separator.getSeparatorChar() && dqCount % 2 == 0)
			{	// if dqCount is even this means we are not inside a quoted value and this is an actual separator
				parts.add(line.substring(startPointer, i));
				startPointer = i + 1;
			}
		}
		
		return parts;
	}
	
	/**
	 * @param valueString
	 * @return
	 * 
	 * @see CSVRecordsExporter#escapeAndQuote(String, boolean)
	 */
	private String deescapeAndUnquote(String valueString)
	{
		return StringUtils.deescapeByDoublingAndWrapping(valueString, DOUBLE_QUOTE);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.Importer#getLastImportExportedAtTime()
	 */
	@Override
	public TimeStamp getLastImportExportedAtTime()
	{
		return exportedAt;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.eximport.Importer#addCustomValueParser(uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ImportHelper.CustomValueParser)
	 */
	public void addCustomValueParser(CustomValueParser customValueParser)
	{
		helper.addCustomValueParser(customValueParser);
	}
	
	/**
	 * Helper class which turns String representations (already de-escaped and unquoted) into column values.
	 * 
	 * @see CSVRecordsExporter#CSVColumnValueStringProvider
	 * @author mstevens
	 */
	private class CSVImportHelper extends ImportHelper
	{

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.eximport.helpers.ImportColumnValueParser#getSubValueSet(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn, java.lang.String)
		 */
		@Override
		protected <VS extends ValueSet<CS>, CS extends ColumnSet> VS getSubValueSet(ValueSetColumn<VS, CS> valueSetCol, String valueString) throws Exception
		{
			if(CSVRecordsExporter.NON_NULL_SUB_VALUESET.equals(valueString))
				return initialise(valueSetCol.getNewValueSet());
			else
				return super.getSubValueSet(valueSetCol, valueString);
		}

	}

}
