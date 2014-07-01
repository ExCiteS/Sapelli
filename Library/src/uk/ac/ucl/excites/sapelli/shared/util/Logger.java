package uk.ac.ucl.excites.sapelli.shared.util;

import java.io.File;

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
	
	public static final String FIELD_SEPARATOR = "; ";
	public static final String LOG_EXTENSION = ".log";
	
	private DateTimeFormatter formatter;
	private FileWriter fileWriter;

	public Logger(String folderPath, String baseFileName)
	{
		this.formatter = ISODateTimeFormat.dateTime();
		try
		{
			this.fileWriter = new FileWriter(folderPath + File.separator + baseFileName + (TimeUtils.getTimestampForFileName()) + LOG_EXTENSION);
			fileWriter.open(FileHelpers.FILE_EXISTS_STRATEGY_APPEND, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}
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

	private String getTime()
	{
		DateTime now = new DateTime();
		return formatter.withZone(now.getZone()).print(now);
	}
	
}
