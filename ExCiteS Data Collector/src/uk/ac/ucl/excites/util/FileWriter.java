package uk.ac.ucl.excites.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import uk.ac.ucl.excites.collector.project.util.FileHelpers;
import android.util.Log;

/**
 * Text-based file writer class (code borrowed from NoiseTube Mobile)
 * 
 * @author mstevens
 * 
 */
public class FileWriter
{

	protected final static String TAG = "FileWriter";

	protected String fullPath;
	protected String characterEncoding = null;

	protected OutputStreamWriter writer = null;

	protected File folder;

	private File file = null;

	public FileWriter(String fullPath)
	{
		this(fullPath, null); // will use system default char encoding
	}

	/**
	 * We create this writer, providing the full filepath (e.g. "/my/path/myfile.raw")
	 * 
	 * @param fullPath
	 *            The full filepath (e.g. "/my/path/myfile.raw")
	 */
	public FileWriter(String fullPath, String characterEncoding)
	{
		this.fullPath = fullPath;
		this.characterEncoding = characterEncoding;
		this.folder = FileHelpers.getFolder(FileHelpers.getFolderPath(fullPath));
	}

	public boolean isWritable()
	{
		return(writer != null);
	}

	public void close()
	{
		if(writer != null)
		{
			try
			{
				writer.close();
			}
			catch(IOException ignore)
			{
			}
			finally
			{
				writer = null;
			}
		}
	}

	public void dispose()
	{
		close();
		_dispose();
	}

	public void write(char charToWrite)
	{
		if(writer != null)
		{
			try
			{
				writer.write(charToWrite);
				writer.flush();
			}
			catch(Exception e)
			{
				Log.d(TAG, "Could not write to file", e);
				close();
			}
		}
	}

	public void write(String stringToWrite)
	{
		if(writer != null)
		{
			try
			{
				writer.write(stringToWrite);
				writer.flush();
			}
			catch(Exception e)
			{
				Log.d(TAG, "Could not write to file", e);
				close();
			}
		}
	}

	public void writeLine(String stringToWrite)
	{
		write(stringToWrite + "\n");
	}

	/**
	 * @return the fullPath
	 */
	public String getFullPath()
	{
		return fullPath;
	}

	/**
	 * @return the characterEncoding
	 */
	public String getCharacterEncoding()
	{
		return characterEncoding;
	}

	@Override
	protected void finalize()
	{
		dispose();
	}

	public void open(int fileExistsStrategy, int fileDoesNotExistStrategy) throws Exception
	{
		if(fullPath == null)
			throw new NullPointerException();
		if(!FileHelpers.isFilePath(fullPath))
			throw new Exception("Not a valid file path (" + fullPath + ")");
		if(fileExistsStrategy < 0 || fileExistsStrategy > 4)
			throw new IllegalArgumentException("Invalid file exists strategy");
		if(fileDoesNotExistStrategy < 1 || fileDoesNotExistStrategy > 2)
			throw new IllegalArgumentException("Invalid file does not exist strategy");
		boolean seekToEOF = false;
		if((new File(fullPath)).exists())
		{ // file already exists
			switch(fileExistsStrategy)
			{
			case (FileHelpers.FILE_EXISTS_STRATEGY_REPLACE):
				break;
			case (FileHelpers.FILE_EXISTS_STRATEGY_REJECT):
				folder = null;
				break;
			case (FileHelpers.FILE_EXISTS_STRATEGY_CREATE_RENAMED_FILE):
				// find a filename that does not exist yet (by adding a counter):
				String extension = FileHelpers.getFileExtension(fullPath);
				String pathWithoutExtension = FileHelpers.trimFileExtensionAndDot(fullPath);
				int i = 1; // counter
				do
				{
					fullPath = pathWithoutExtension + "-" + i + "." + extension;
					i++;
				}
				while((new File(fullPath)).exists()); // try until non-existing file found
				break;
			case (FileHelpers.FILE_EXISTS_STRATEGY_APPEND):
				seekToEOF = true;
				break;
			}
		}
		else
		{ // file does not exist
			switch(fileDoesNotExistStrategy)
			{
			case (FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_REJECT):
				folder = null;
				break;
			case (FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE):
				folder.mkdirs(); // file will be created lower, but we need to make sure the folder is created here.
				break;
			}
		}
		if(folder != null)
		{
			// Create file:
			File file = new File(folder, FileHelpers.getFileName(fullPath));

			// Open file:
			OutputStream outputStream = new FileOutputStream(file, seekToEOF);
			writer = ((characterEncoding == null || characterEncoding.equals("")) ? new OutputStreamWriter(outputStream) : new OutputStreamWriter(outputStream,
					characterEncoding));
		}
		else
			throw new Exception("Could not open FileWriter");
	}

	protected void _dispose()
	{
		file = null;
		folder = null;
	}

	public void rename(String newName, int fileExistsStrategy) throws Exception
	{ // TODO make sure the FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE is ok?
		if(writer != null)
			close();
		fullPath = FileHelpers.getFolderPath(fullPath) + newName;
		file.renameTo(new File(fullPath));
	}

	public void delete() throws Exception
	{
		if(writer != null)
			close();
		file.delete();
	}

	public String getContainingFolderPath()
	{
		return FileHelpers.getFolderPath(fullPath);
	}

	public String getFileName()
	{
		return FileHelpers.getFileName(fullPath);
	}

	public boolean fileExists()
	{
		if(file != null)
			return file.exists();
		else
			throw new IllegalStateException("File is null, don't use this FileWriter");
	}

	public long fileLastChanged()
	{
		if(file != null)
			return file.lastModified();
		else
			throw new IllegalStateException("File is null, don't use this FileWriter");
	}

}