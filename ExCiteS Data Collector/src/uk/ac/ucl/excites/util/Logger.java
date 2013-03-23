package uk.ac.ucl.excites.util;

import java.io.File;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import uk.ac.ucl.excites.collector.project.util.FileHelpers;
import uk.ac.ucl.excites.collector.util.Debug;

public class Logger
{
	public static final String FIELD_SEPARATOR = "; ";
	public static final String LOG_EXTENSION = ".log";
	private DateTimeFormatter formatter;
	private FileWriter fileWriter;

	public Logger(String filePath)
	{
		this.formatter = ISODateTimeFormat.dateTime();
		this.fileWriter = new FileWriter(filePath + File.separator + System.currentTimeMillis() + LOG_EXTENSION);

		try
		{
			this.fileWriter.open(FileHelpers.FILE_EXISTS_STRATEGY_APPEND, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
	}

	/**
	 * Add a new line with the following format: TIMESTAMP;MSG;
	 * 
	 * @param log
	 */
	public void addLog(String log)
	{
		fileWriter.writeLine(getTime() + FIELD_SEPARATOR + log + FIELD_SEPARATOR);
	}

	/**
	 * 
	 * @param logs
	 */
	public void addLog(String... logs)
	{
		fileWriter.write(getTime());
		for(String log : logs)
		{
			fileWriter.write(FIELD_SEPARATOR + log);
		}

		fileWriter.writeLine(FIELD_SEPARATOR);
	}

	/**
	 * Adds the final line to the Logger
	 * 
	 * @param log
	 */
	public void addFinalLog(String log)
	{
		addLog(log);
		addWhiteSpace();
		fileWriter.dispose();
	}

	public void addFinalLog(String... logs)
	{
		addLog(logs);
		addWhiteSpace();
		fileWriter.dispose();
	}

	/**
	 * Add some whitespace
	 */
	public void addWhiteSpace()
	{
		fileWriter.writeLine("");
	}

	private String getTime()
	{
		DateTime now = new DateTime();
		return formatter.withZone(now.getZone()).print(now);
	}
}
