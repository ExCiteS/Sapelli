package uk.ac.ucl.excites.sapelli.shared.util.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

/**
 * Text-based file writer class (some code borrowed from NoiseTube Mobile; Licensed under LGPL v2.1)
 * 
 * @author mstevens
 * 
 */
public class FileWriter
{

	//protected String fullPath;
	protected Charset charset; 

	protected OutputStreamWriter writer = null;
	protected StringBuffer transactionBuffer = null; 

	private File file = null;

	public FileWriter(String fullPath)
	{
		this(fullPath, Charset.defaultCharset()); // will use system default char encoding
	}

	/**
	 * We create this writer, providing the full filepath (e.g. "/my/path/myfile.raw")
	 * 
	 * @param fullPath
	 *            The full filepath (e.g. "/my/path/myfile.raw")
	 */
	public FileWriter(String fullPath, String characterEncoding)
	{
		this(fullPath, Charset.forName(characterEncoding));
	}
	
	/**
	 * We create this writer, providing the full filepath (e.g. "/my/path/myfile.raw")
	 * 
	 * @param fullPath
	 *            The full filepath (e.g. "/my/path/myfile.raw")
	 */
	public FileWriter(String fullPath, Charset charset)
	{
		if(fullPath == null)
			throw new NullPointerException("fullPath cannot be null");
		if(charset == null)
			throw new NullPointerException("charSet cannot be null");
		this.file = new File(fullPath);
		this.charset = charset;
	}

	public boolean isWritable()
	{
		return(writer != null);
	}

	public void close()
	{
		rollbackTransaction(); // rollback any non-committed transaction
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
		write(new String(new char[] { charToWrite }));
	}

	public void write(String stringToWrite)
	{
		if(transactionBuffer != null)
		{	// we are in transaction mode: buffer output
			transactionBuffer.append(stringToWrite);
		}
		else if(writer != null)
		{
			try
			{
				writer.write(stringToWrite);
				writer.flush();
			}
			catch(Exception e)
			{
				System.err.println("FileWriter: Could not write to file: " + e.getMessage());
				e.printStackTrace(System.err);
				close();
			}
		}
	}

	public void writeLine(String stringToWrite)
	{
		write(stringToWrite + "\n");
	}
	
	public void openTransaction()
	{
		if(transactionBuffer == null) // we currently only support a single open transaction at the time
			transactionBuffer = new StringBuffer();
	}
	
	public void commitTransaction()
	{
		if(transactionBuffer != null)
		{
			String output = transactionBuffer.toString();
			transactionBuffer = null;
			write(output);
		}
	}
	
	public boolean isTransactionBufferEmpty()
	{
		return transactionBuffer == null || transactionBuffer.length() == 0;
	}
	
	public void rollbackTransaction()
	{
		transactionBuffer = null; // discard transactionBuffer and its contents!
	}

	/**
	 * @return the fullPath
	 */
	public String getFullPath()
	{
		return file.getAbsolutePath();
	}

	@Override
	protected void finalize()
	{
		dispose();
	}

	public void open(int fileExistsStrategy, int fileDoesNotExistStrategy) throws IOException
	{
		if(fileExistsStrategy < 0 || fileExistsStrategy > 4)
			throw new IllegalArgumentException("Invalid file exists strategy");
		if(fileDoesNotExistStrategy < 1 || fileDoesNotExistStrategy > 2)
			throw new IllegalArgumentException("Invalid file does not exist strategy");
		boolean seekToEOF = false;
		if(file.exists())
		{ // file already exists
			switch(fileExistsStrategy)
			{
			case (FileHelpers.FILE_EXISTS_STRATEGY_REPLACE):
				break;
			case (FileHelpers.FILE_EXISTS_STRATEGY_REJECT):
				throw new IOException("Could not open FileWriter, file already exists");
			case (FileHelpers.FILE_EXISTS_STRATEGY_CREATE_RENAMED_FILE):
				// find a filename that does not exist yet (by adding a counter):
				String extension = FileHelpers.getFileExtension(file);
				String pathWithoutExtension = FileHelpers.trimFileExtensionAndDot(file.getAbsolutePath());
				int i = 1; // counter
				do
				{
					file = new File(pathWithoutExtension + "-" + i + "." + extension);
					i++;
				}
				while(file.exists()); // try until non-existing file found
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
				throw new IOException("Could not open FileWriter, file does not exist");
			case (FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE):
				file.getParentFile().mkdirs(); // file will be created lower, but we need to make sure the folder is created here.
				file.createNewFile();
			}
		}
		
		// Open file:
		writer = new OutputStreamWriter(new FileOutputStream(file, seekToEOF), charset);

	}

	protected void _dispose()
	{
		file = null;
	}

	public void rename(String newName, int fileExistsStrategy) throws Exception
	{ // TODO make sure the FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE is ok?
		if(writer != null)
			close();
		file.renameTo(new File(getContainingFolderPath() + newName));
	}

	/**
	 * Delete the file being written to (first closing the writer)
	 * 
	 */
	public void delete()
	{
		if(writer != null)
			close();
		file.delete();
	}

	public String getContainingFolderPath()
	{
		return FileHelpers.getFolderPath(file.getAbsolutePath());
	}

	public String getFileName()
	{
		return FileHelpers.getFileName(file.getAbsolutePath());
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