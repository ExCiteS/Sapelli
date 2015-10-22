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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.io.File;
import java.io.IOException;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.text.FileWriter;


/**
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class Logger
{
	
	public static final String FIELD_SEPARATOR = ";";
	public static final String LOG_EXTENSION = ".log";
	
	private DateTimeFormatter formatter;
	private FileWriter fileWriter;
	protected final boolean printToOutputStream;
	
	/**
	 * 
	 * @param folderPath path to the folder in which the log file is saved
	 * @param baseFileName base filename for the log file (will be suffixed by a timestamp)
	 * @param printToOutputStream whether or not to also echo log statements to the standard output stream (e.g. System.out or Android Logcat)
	 * @throws IOException from file system I/O
	 */
	public Logger(String folderPath, String baseFileName, boolean printToOutputStream) throws IOException
	{
		this(folderPath, baseFileName, true, printToOutputStream);
	}
	
	/**
	 * 
	 * @param folderPath path to the folder in which the log file is saved
	 * @param baseFileName base filename for the log file
	 * @param timestampFilename whether or not to suffix the base filename with a timestamp (precise to the second of file creation)
	 * @param printToOutputStream whether or not to also echo log statements to the standard output stream (e.g. System.out or Android Logcat)
	 * @throws IOException from file system I/O
	 */
	public Logger(String folderPath, String baseFileName, boolean timestampFilename, boolean printToOutputStream) throws IOException
	{
		this.formatter = ISODateTimeFormat.dateTime();
		this.fileWriter = new FileWriter(folderPath + File.separator + baseFileName + (timestampFilename ? TimeUtils.getTimestampForFileName() : "") + LOG_EXTENSION);
		this.printToOutputStream = printToOutputStream;
		fileWriter.open(FileHelpers.FILE_EXISTS_STRATEGY_APPEND, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
	}

	/**
	 * Add a new line with the following format: TIMESTAMP;MSG
	 * 
	 * @param line
	 */
	public void addLine(String line)
	{
		addLine(line, true);
	}
	
	/**
	 * Add a new line with format:
	 * 	- when timestamp=true: TIMESTAMP;MSG
	 * 	- when timestamp=false: MSG
	 * 
	 * @param line
	 * @param timestamp whether or not to include a timestamp
	 */
	public void addLine(String line, boolean timestamp)
	{
		addLine(timestamp, new String[] { line });
	}

	/**
	 * Add a new line with the following format: TIMESTAMP;fields[0];...;fields[fields.length-1]
	 * 
	 * @param fields
	 */
	public void addLine(String... fields)
	{
		addLine(true, fields);
	}
	
	/**
	 * Add a new line with format:
	 * 	- when timestamp=true: TIMESTAMP;fields[0];...;fields[fields.length-1]
	 * 	- when timestamp=false: fields[0];...;fields[fields.length-1]
	 * 
	 * @param timestamp
	 * @param fields
	 */
	public void addLine(boolean timestamp, String... fields)
	{
		TransactionalStringBuilder bff = new TransactionalStringBuilder(FIELD_SEPARATOR);
		if(timestamp)
			bff.append(getTime());
		if(fields != null)
			for(String field : fields)
				bff.append(field);
		
		// To stream:
		printToOutputStream(bff.toString());
		// To file:
		writeLine(bff.toString());
	}

	/**
	 * Add some whitespace
	 */
	public void addBlankLine()
	{
		// To stream: nope, don't reproduce blank lines on System.out
		// To file:
		writeLine("");
	}
	
	/**
	 * Adds the final line to the Logger
	 * 
	 * @see Logger#addLine(String...)
	 * @param fields
	 */
	public void addFinalLine(String... fields)
	{
		addLine(fields); // will also print to output stream if appropriate
		addBlankLine();
		close();
	}
	
	private void writeLine(String str)
	{
		if(fileWriter == null || !fileWriter.isWritable())
			throw new IllegalStateException("Logger " + fileWriter.getFullPath() + "has been closed or file is not writable.");
		fileWriter.writeLine(str);
	}
	
	protected void printToOutputStream(String line)
	{
		if(printToOutputStream)
			System.out.println(line);
	}
	
	@Override
	public void finalize() throws Throwable
	{
		close();
		super.finalize();
	}

	/**
	 * Closes the log file. Nothing can be added to it after this method has been called.
	 */
	public void close()
	{
		if(fileWriter != null)
		{
			fileWriter.dispose();
			fileWriter = null;
		}
	}

	/**
	 * Return the time in two formats, one in ISO8601 and one in "pretty ISO" format ("yyyy-MM-dd HH:mm:ss"), which should be correctly interpreted by (most)
	 * Excel installations.
	 * 
	 * @return
	 */
	private String getTime()
	{
		DateTime now = new DateTime();
		return formatter.withZone(now.getZone()).print(now) + FIELD_SEPARATOR + TimeUtils.PrettyTimestampWithoutMSFormatter.print(now);
	}

}
