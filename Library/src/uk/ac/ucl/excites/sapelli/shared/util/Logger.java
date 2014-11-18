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
import uk.ac.ucl.excites.sapelli.shared.io.FileWriter;


/**
 * @author Michalis Vitos, mstevens
 *
 */
public class Logger
{
	
	public static final String FIELD_SEPARATOR = ";";
	public static final String LOG_EXTENSION = ".log";
	
	private DateTimeFormatter formatter;
	private FileWriter fileWriter;

	public Logger(String folderPath, String baseFileName) throws IOException
	{
		this.formatter = ISODateTimeFormat.dateTime();
		this.fileWriter = new FileWriter(folderPath + File.separator + baseFileName + (TimeUtils.getTimestampForFileName()) + LOG_EXTENSION);
		fileWriter.open(FileHelpers.FILE_EXISTS_STRATEGY_APPEND, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
	}

	/**
	 * Add a new line with the following format: TIMESTAMP;MSG;
	 * 
	 * @param line
	 */
	public void addLine(String line)
	{
		checkWriter();
		fileWriter.writeLine(getTime() + FIELD_SEPARATOR + line + FIELD_SEPARATOR);
	}

	/**
	 * Add a new line with the following format: TIMESTAMP;fields[0];...;fields[fields.length-1]
	 * 
	 * @param fields
	 */
	public void addLine(String... fields)
	{
		checkWriter();
		fileWriter.write(getTime());
		for(String field : fields)
			fileWriter.write(FIELD_SEPARATOR + field);
		fileWriter.writeLine(FIELD_SEPARATOR);
	}

	/**
	 * Adds the final line to the Logger
	 * 
	 * @see Logger#addLine(String...)
	 * @param fields
	 */
	public void addFinalLine(String... fields)
	{
		addLine(fields);
		addBlankLine();
		close();
	}

	/**
	 * Add some whitespace
	 */
	public void addBlankLine()
	{
		checkWriter();
		fileWriter.writeLine("");
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

	private void checkWriter()
	{
		if(fileWriter == null || !fileWriter.isWritable())
			throw new IllegalStateException("Logger " + fileWriter.getFullPath() + "has been closed or file is not writable.");
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
