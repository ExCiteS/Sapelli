/*
 * Copyright (c) 2011 Matthew Francis
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package demo;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.itadaki.bzip2.BZip2InputStream;


/**
 * A BZip2 file decompressor. For demonstration purposes only
 */
public class Decompress {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main (String[] args) throws IOException {

		if (args.length == 0) {
			System.err.println ("Demonstration BZip2 decompressor\n\nUsage:\n  java demo.Decompress <filename>\n");
			System.exit (1);
		}

		File inputFile = new File (args[0]);
		if (!inputFile.exists() || !inputFile.canRead() || !args[0].endsWith(".bz2")) {
			System.err.println ("Cannot read file " + inputFile.getPath());
			System.exit (1);
		}

		File outputFile = new File (args[0].substring (0, args[0].length() - 4));
		if (outputFile.exists()) {
			System.err.println ("File " + outputFile.getPath() + " already exists");
			System.exit (1);
		}

		InputStream fileInputStream = new BufferedInputStream (new FileInputStream (inputFile));
		BZip2InputStream inputStream = new BZip2InputStream (fileInputStream, false);
		OutputStream fileOutputStream = new BufferedOutputStream (new FileOutputStream (outputFile), 524288);

		byte[] decoded = new byte [524288];
		int bytesRead;
		while ((bytesRead = inputStream.read (decoded)) != -1) {
			fileOutputStream.write (decoded, 0, bytesRead) ;	
		}
		fileOutputStream.close();

	}

}
