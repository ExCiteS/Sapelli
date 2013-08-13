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

import org.itadaki.bzip2.BZip2OutputStream;


/**
 * A BZip2 file compressor. For demonstration purposes only
 */
public class Compress {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main (String[] args) throws IOException {

		if (args.length == 0) {
			System.err.println ("Demonstration BZip2 compressor\n\nUsage:\n  java demo.Compress <filename>\n");
			System.exit (1);
		}

		File inputFile = new File (args[0]);
		if (!inputFile.exists() || !inputFile.canRead()) {
			System.err.println ("Cannot read file " + inputFile.getPath());
			System.exit (1);
		}

		File outputFile = new File (args[0] + ".bz2");
		if (outputFile.exists()) {
			System.err.println ("File " + outputFile.getPath() + " already exists");
			System.exit (1);
		}

		InputStream fileInputStream = new BufferedInputStream (new FileInputStream (inputFile));
		OutputStream fileOutputStream = new BufferedOutputStream (new FileOutputStream (outputFile), 524288);
		BZip2OutputStream outputStream = new BZip2OutputStream (fileOutputStream);

		byte[] buffer = new byte [524288];
		int bytesRead;
		while ((bytesRead = fileInputStream.read (buffer)) != -1) {
			outputStream.write (buffer, 0, bytesRead);
		}
		outputStream.close();

	}

}
