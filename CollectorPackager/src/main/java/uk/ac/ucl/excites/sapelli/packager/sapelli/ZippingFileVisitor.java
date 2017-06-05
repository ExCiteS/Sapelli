package uk.ac.ucl.excites.sapelli.packager.sapelli;


import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;

/**
 * {@link uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor} to create a list of files that should be zipped and create the Sapelli .sap file
 * <p>
 * Created by Michalis on 05/06/2017.
 */
@Slf4j
public class ZippingFileVisitor implements FileVisitor<Path>
{
	// List of files to ignore during zipping the project
	// Batch files: .bat
	// Archives: .zip, .7z, .rar, .zipx
	// Illustrator, Photoshop: .ai, .psd
	// Sapelli archives: .sap, .sapelli, .excites
	public static String[] IGNORED_EXTENSIONS = new String[]{
	  "bat",
	  "zip", "7z", "rar", "zipx",
	  "ai", "psd",
	  "sap", "sapelli", "excites"
	};

	@Getter
	private List<File> filesToZip;
	private File rootDir;

	public ZippingFileVisitor(File rootDir)
	{
		this.rootDir = rootDir;
		filesToZip = new ArrayList<>();
	}

	@Override
	public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException
	{
		// Check if the current dir is the rootDir
		boolean root = dir.toAbsolutePath().normalize().toString().equalsIgnoreCase(rootDir.getAbsolutePath());

		// Check for Sapelli directories
		final String dirFileName = dir.getFileName().toString();
		boolean img = dirFileName.equalsIgnoreCase(FileStorageProvider.IMAGE_FOLDER);
		boolean snd = dirFileName.equalsIgnoreCase(FileStorageProvider.SOUND_FOLDER);
		boolean resources = dirFileName.equalsIgnoreCase(FileStorageProvider.RES_FOLDER);


		// ROOT: CONTINUE
		if(root)
		{
			// Continue inside this directory
			return FileVisitResult.CONTINUE;
		}
		// SAPELLI DIR: STORE & CONTINUE
		else if(img || snd || resources)
		{
			// Keep file
			filesToZip.add(dir.toFile());

			// Continue inside this directory
			return FileVisitResult.CONTINUE;
		}
		// OTHER: IGNORE
		else
		{
			// Continue inside this directory
			return FileVisitResult.SKIP_SUBTREE;
		}
	}

	@Override
	public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException
	{
		// Do nothing
		return FileVisitResult.CONTINUE;
	}

	@Override
	public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException
	{
		// Check if file matches the IGNORED_EXTENSIONS
		for(String ext : IGNORED_EXTENSIONS)
		{
			// Ignore file if it matches the extension
			if(ext.equalsIgnoreCase(FileHelpers.getFileExtension(file.toFile())))
				return FileVisitResult.CONTINUE;
		}

		// Otherwise keep it on the filesToZip
		filesToZip.add(file.toFile());

		return FileVisitResult.CONTINUE;
	}

	@Override
	public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException
	{
		log.error("VisitFileFailed: {}  ::  {}", file, exc);
		return FileVisitResult.CONTINUE;
	}

}