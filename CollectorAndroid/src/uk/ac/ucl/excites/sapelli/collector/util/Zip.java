package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.util.Debug;

public class Zip
{
	private static final int BUFFER_SIZE = 2048;

	private ZipOutputStream zip;
	private BufferedInputStream origin;
	private byte data[];

	private String[] folders;
	private String zipFileDest;

	public Zip(String[] folders, String zipFileDest)
	{
		this.folders = folders;
		// TODO check if .zip
		this.zipFileDest = zipFileDest;
	}

	public void zip() throws FileNotFoundException, IOException
	{
		// Create the ZipOutputStream
		if(zip == null)
			zip = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipFileDest)));

		// Iterate through all files and add them to the zip file
		for(String f : folders)
			if(f != null)
				zipFile(f);

		// Close the ZipOutputStream
		zip.close();
	}

	/**
	 * Zips a file to a zip file saved on
	 * 
	 * @param sourceFilePath
	 * @param zipFileDest
	 * @return
	 */
	private boolean zipFile(String sourceFilePath)
	{
		// Create and test the file
		File sourceFile = null;
			sourceFile = new File(sourceFilePath);
		
		if(!sourceFile.exists())
			return false;

		// Try to zip it
		try
		{
			// Check if file is a directory and use zipSubFolder()
			if(sourceFile.isDirectory())
			{
				zipSubFolder(sourceFile, sourceFile.getParent().length());
			}
			else
			{
				data = new byte[BUFFER_SIZE];
				FileInputStream fi = new FileInputStream(sourceFilePath);
				origin = new BufferedInputStream(fi, BUFFER_SIZE);
				ZipEntry entry = new ZipEntry(FileHelpers.getFileName(sourceFilePath));
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER_SIZE)) != -1)
				{
					zip.write(data, 0, count);
				}
			}
		}
		catch(Exception e)
		{
			Debug.e(e);
			return false;
		}
		return true;
	}

	/**
	 * Zips a subfolder
	 * 
	 * @param folder
	 * @param basePathLength
	 * @throws IOException
	 */
	private void zipSubFolder(File folder, int basePathLength) throws IOException
	{
		File[] fileList = folder.listFiles();
		for(File file : fileList)
		{
			if(file.isDirectory())
			{
				zipSubFolder(file, basePathLength);
			}
			else
			{
				data = new byte[BUFFER_SIZE];
				String unmodifiedFilePath = file.getPath();
				String relativePath = unmodifiedFilePath.substring(basePathLength);
				Debug.d("Relative Path : " + relativePath);
				FileInputStream fi = new FileInputStream(unmodifiedFilePath);
				origin = new BufferedInputStream(fi, BUFFER_SIZE);
				ZipEntry entry = new ZipEntry(relativePath);
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER_SIZE)) != -1)
				{
					zip.write(data, 0, count);
				}
			}
		}
	}
}
