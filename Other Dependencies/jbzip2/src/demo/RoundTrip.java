package demo;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.itadaki.bzip2.BZip2InputStream;
import org.itadaki.bzip2.BZip2OutputStream;


/**
 * Tests the compression/decompression cycle on a file or directory. Compression is performed
 * using a temporary file, and the target file or directory is not altered.
 */
public class RoundTrip {

	/**
	 * A FileFilter that returns only ordinary, readable files
	 */
	private static final FileFilter fileFilter = new FileFilter() {

		@Override
		public boolean accept (File file) {
			return file.isFile() && file.canRead();
		}

	};


	/**
	 * A FileFilter that returns only readable directories
	 */
	private static final FileFilter directoryFilter = new FileFilter() {

		@Override
		public boolean accept (File file) {
			return file.isDirectory() && file.canRead();
		}

	};


	/**
	 * Recursively enumerates ordinary, readable files
	 * @param base The base file or directory name
	 * @return A list of ordinary, readable files
	 */
	private static List<File> getFiles (File base) {

		List<File> files = new ArrayList<File>();

		if (base.isFile()) {

			if (base.canRead()) {
				files.add (base);
			}

		} else {

			File[] subDirectories = base.listFiles (directoryFilter);
			for (File subDirectory : subDirectories) {
				files.addAll (getFiles (subDirectory));
			}

			for (File file : base.listFiles (fileFilter)) {
				files.add (file);
			}

		}

		return files;

	}


	/**
	 * Compresses and decompresses each of a list of files, using a temporary file to hold the compressed data
	 * @param files The files to compress
	 * @throws IOException On any error compressing or decompressing the files
	 */
	private static void roundTrip (List<File> files) throws IOException {

		File tempFile = File.createTempFile ("rtr", ".tmp");

		for (File inputFile : files) {

			long inputLength = inputFile.length();
			System.out.print (inputLength + " " + inputFile);

			// Compress file
			InputStream fileInputStream = new BufferedInputStream (new FileInputStream (inputFile));
			OutputStream compressedOutputStream = new BufferedOutputStream (new FileOutputStream (tempFile));
			BZip2OutputStream bzip2OutputStream = new BZip2OutputStream (compressedOutputStream);

			byte[] buffer = new byte [524288];
			int bytesRead;
			while ((bytesRead = fileInputStream.read (buffer, 0, buffer.length)) != -1) {
				bzip2OutputStream.write (buffer, 0, bytesRead);
			}
			bzip2OutputStream.close();
			compressedOutputStream.close();
			fileInputStream.close();

			long compressedLength = tempFile.length();
			System.out.printf(" (%.1f%%)\n", ((float)(inputLength - compressedLength) / inputLength) * 100);

			// Decompress file
			InputStream compressedInputStream = new BufferedInputStream (new FileInputStream (tempFile));
			BZip2InputStream bzip2InputStream = new BZip2InputStream (compressedInputStream, false);
			byte[] decoded = new byte [524288];
			while ((bytesRead = bzip2InputStream.read (decoded)) != -1);

			compressedInputStream.close();
			bzip2InputStream.close();

		}

	}


	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {

		if (args.length == 0) {
			System.err.println (
					"Tests the compression/decompression cycle on a file or directory. Compression is" +
					"performed using a temporary file, and the target file or directory is not altered.\n\n" +
					"Usage:\n  java demo.RoundTrip <file or directory name>\n");
			System.exit (1);
		}

		System.out.println ("Finding files...");
		List<File> files = getFiles (new File (args[0]));

		System.out.println ("Testing compression/decompression cycle...");
		roundTrip(files);

	}


}
