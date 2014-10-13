package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.util.Debug;

public class Zip
{
	private static final int BUFFER = 2048;

	private ZipOutputStream zip;

	private String[] folders;
	private String zipFileDest;

	public Zip(String[] folders, String zipFileDest) throws Exception
	{
		this.folders = folders;
		// TODO check if .zip
		this.zipFileDest = zipFileDest;
	}

	public void zip()
	{
		for(String f : folders)
			if(f != null)
				zipFile(f, zipFileDest);
	}

	/**
	 * Zips a file to a zip file saved on
	 * 
	 * @param sourceFileDest
	 * @param zipFileDest
	 * @return
	 */
	private boolean zipFile(String sourceFileDest, String zipFileDest)
	{
		// Create and test the file
		File sourceFile = null;
		try
		{
			sourceFile = new File(sourceFileDest);
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
		
		if(sourceFile == null)
			return false;

		// Try to zip it
		try
		{
			BufferedInputStream origin = null;
			FileOutputStream dest = new FileOutputStream(zipFileDest);
			zip = new ZipOutputStream(new BufferedOutputStream(dest));

			// Check if file is a directory and use zipSubFolder()
			if(sourceFile.isDirectory())
			{
				zipSubFolder(sourceFile, sourceFile.getParent().length());
			}
			else
			{
				byte data[] = new byte[BUFFER];
				FileInputStream fi = new FileInputStream(sourceFileDest);
				origin = new BufferedInputStream(fi, BUFFER);
				ZipEntry entry = new ZipEntry(FileHelpers.getFileName(sourceFileDest));
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER)) != -1)
				{
					zip.write(data, 0, count);
				}
			}
			zip.close();
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
		BufferedInputStream origin = null;
		for(File file : fileList)
		{
			if(file.isDirectory())
			{
				zipSubFolder(file, basePathLength);
			}
			else
			{
				byte data[] = new byte[BUFFER];
				String unmodifiedFilePath = file.getPath();
				String relativePath = unmodifiedFilePath.substring(basePathLength);
				Debug.d("Relative Path : " + relativePath);
				FileInputStream fi = new FileInputStream(unmodifiedFilePath);
				origin = new BufferedInputStream(fi, BUFFER);
				ZipEntry entry = new ZipEntry(relativePath);
				zip.putNextEntry(entry);
				int count;
				while((count = origin.read(data, 0, BUFFER)) != -1)
				{
					zip.write(data, 0, count);
				}
				origin.close();
			}
		}
	}
}
