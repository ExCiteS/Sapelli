/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.io.text;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.io.Charsets;
import org.apache.commons.io.filefilter.WildcardFileFilter;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ClassHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.IterableEnumeration;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;

/**
 * <p>
 * Helper class which provides consistent (across different Java VMs/runtimes) and conservative (i.e., possibly
 * overestimated) values for the maximum number of bytes used per char(acter) when encoding {@link String}s with
 * any supported {@link Charset}s.</p>
 * <p>
 * Created on 2015-10-21 when I learned that the result of {@code charset.newEncoder().maxBytesPerChar()}
 * can vary across different Java VM/runtimes. For example, on the Oracle JRE v1.8.0_x and v1.7.0_x we get
* {@code 3.0f} for UTF-8 (as on all Android versions as far as I've been able to test), but on JRE v1.6.0_x
 * it was {@code 4.0f} (which was arguably wrong). See:<br/>
 * 	 - <a href="http://bugs.java.com/view_bug.do?bug_id=6957230">http://bugs.java.com/view_bug.do?bug_id=6957230</a><br/>
 *   - <a href="http://bugs.java.com/view_bug.do?bug_id=8058875">http://bugs.java.com/view_bug.do?bug_id=8058875</a><br/>
 * Further research showed that there are also differences, in the reported maximum bytes per char for certain encodings,
 * between the Oracle JRE and Android.</p>
 * <p>
 * All of this is problematic for our StringColumn class which needs a consistent {@code maxBytesPerChar} value
 * to compute the maximum number of bytes Strings of a certain number of chars will require when encoded using
 * a given {@link Charset}. Hence I created this class to solve this problem.</p>
 * <p>
 * The {@link #GetMaxBytesPerChar(Charset)} and {@link #GetMaxBytesPerChar(String)} methods provide the maximum
 * {@code maxBytesPerChar} value, across several Java VM/runtime implementations/versions, for the a given
 * {@link Charset} (name). The {@code maxBytesPerChar} values for different {@code Charset}s as implemented on
 * different VMs/runtimes are loaded from a properties file (named {@value #CMMBPC_PROPERTIES_FILE_NAME}) which
 * must reside in the same package as this class.</p>
 * <p>
 * The {@link #WriteCharsetInfoFile(File, String, String)} method writes out a ".charsetinfo" file with the
 * {@code maxBytesPerChar} values for all {@link Charset}s supported by the current platform/device.</p>
 * <p>
 * The {@link #GeneratePropertiesFile(String, String, boolean) method loads multiple ".charsetinfo" files
 * and summarises the information (i.e. finding the maximum {@code maxBytesPerChar} for each {@link Charset})
 * and writes it to a new {@value #CMMBPC_PROPERTIES_FILE_NAME} properties file, to be packaged with the
 * library for the usage by {@link #GetMaxBytesPerChar(Charset)} and {@link #GetMaxBytesPerChar(String)} methods.</p>
 * 
 * @author mstevens
 */
public final class CharsetHelpers
{
	
	private CharsetHelpers() {}
	
	private static final Charset CHARSETINFO_FILE_CHARSET = Charsets.UTF_8;
	
	public static final String CHARSETINFO_FILE_EXTENSION = "charsetinfo";
	
	private static final Charset PROPERTIES_FILE_CHARSET = Charsets.ISO_8859_1;
	
	public static final String PROPERTIES_FILE_EXTENSION = "properties";
	
	private static final char COMMENT_LINE_MARKER = '#';
	
	private static final String EOL = System.getProperty("line.separator");
	
	public static final String CMMBPC_PROPERTIES_FILE_NAME = "CharsetMaxMaxBytesPerChar";
	
	private static final ResourceBundle CharsetMaxMaxBytesPerCharBundle;
	static
	{
		ResourceBundle bundle = null;
		try
		{
			bundle = ResourceBundle.getBundle(ClassHelpers.classPackage(CharsetHelpers.class) + "." + CMMBPC_PROPERTIES_FILE_NAME);
			/*if(bundle != null)
				System.out.println("Got bundle, UTF-8 maxBytesPerChar:" + bundle.getString(Charsets.UTF_8.name()));*/
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			System.err.println("Could not load resource bundle (" + CMMBPC_PROPERTIES_FILE_NAME + ".properties" + ")");
		}
		CharsetMaxMaxBytesPerCharBundle = bundle;
	}
	
	private static int getNumberOfKnownCharsets()
	{
		int count = 0;
		if(CharsetMaxMaxBytesPerCharBundle != null)
			for(@SuppressWarnings("unused") String charsetName : IterableEnumeration.Make(CharsetMaxMaxBytesPerCharBundle.getKeys()))
				count++;
		return count;
	}
	
	/**
	 * Returns the maximum "maxBytesPerChar" value for the given {@link Charset},
	 * across all Java VM/runtime implementations/versions we know about.
	 * 
	 * @param charset
	 * @return
	 * @throws IllegalArgumentException
	 */
	static public final float GetMaxBytesPerChar(Charset charset) throws IllegalArgumentException
	{
		float maxBytesPerChar = 0.0f;
		try
		{
			maxBytesPerChar = GetMaxBytesPerChar(charset.name());
		}
		catch(IllegalArgumentException iae)
		{	// Fall-back (determine now):
			maxBytesPerChar = new CharsetInfo(charset).maxBytesPerChar;
		}
		// Check:
		if(maxBytesPerChar <= 0.0f)
			throw new IllegalArgumentException("maxBytesPerChar for Charset \"" + charset.name() + "\" is indeterminable.");
		//else:
			return maxBytesPerChar;
	}
	
	/**
	 * Returns the maximum "maxBytesPerChar" value for the {@link Charset} with the given name,
	 * across all Java VM/runtime implementations/versions we know about.
	 * 
	 * @param charsetName
	 * @return
	 * @throws IllegalArgumentException
	 */
	static public final float GetMaxBytesPerChar(String charsetName) throws IllegalArgumentException
	{
		float maxBytesPerChar = 0.0f;
		if(CharsetMaxMaxBytesPerCharBundle != null && CharsetMaxMaxBytesPerCharBundle.containsKey(charsetName))
			// Consult resource bundle:
			maxBytesPerChar = Float.valueOf(CharsetMaxMaxBytesPerCharBundle.getString(charsetName));
		// Check:
		if(maxBytesPerChar <= 0.0f)
			throw new IllegalArgumentException("maxBytesPerChar for Charset \"" + charsetName + "\" is indeterminable.");
		//else:
			return maxBytesPerChar;
	}
	
	/**
	 * Writes a .charsetinfo file containing information about all supported {@link Charset}s on the current machine/platform.
	 * 
	 * @param outputFolder
	 * @param filename the name of the output file, without extension
	 * @param header
	 * @throws IOException
	 */
	static public final void WriteCharsetInfoFile(File outputFolder, String filename, String header) throws IOException
	{
		if(!FileHelpers.createDirectory(outputFolder))
			throw new IOException("Could not create output directory.");
		try(FileOutputStream fos = new FileOutputStream(new File(outputFolder, filename + "." + CHARSETINFO_FILE_EXTENSION));
			BufferedWriter writer = new BufferedWriter(UnicodeBOM.GetWriter(fos, CHARSETINFO_FILE_CHARSET)))
		{
			// Header:
			if(header != null)
			{
				if(!header.isEmpty())
					writer.write(COMMENT_LINE_MARKER + " " + header.trim() + EOL);
			}
			// Body:
			for(Charset cs : Charset.availableCharsets().values())
				writer.write(new CharsetInfo(cs).toString() + EOL);
		}
	}
	
	/**
	 * Parses all .charsetinfo files in the input folder and works out which is the 
	 * maximum "maxBytesPerChar" value for each known Charset across all input files.
	 * If this summarised information is in anyway different from the information in
	 * {@link #CharsetMaxMaxBytesPerCharBundle} (or if {@code force} is {@code true})
	 * then a new CharsetMaxMaxBytesPerChar.properties is created (or overwritten!)
	 * in the output folder.
	 * 
	 * This method is called from a Groovy script in the pom.xml of the Sapelli Library.
	 * 
	 * @param inputFolderPath path of a directory containing CharsetInfo files (with *.charsetinfo extension!) to process
	 * @param outputFolderPath path of a (resource) directory in which to create the new/updated CharsetMaxMaxBytesPerChar.properties file
	 * @param force when {@code true} a new CharsetMaxMaxBytesPerChar.properties file will always be (re)generated
	 * @return whether or not a new or updated CharsetMaxMaxBytesPerChar.properties file was created
	 * @throws IOException
	 */
	static public final boolean GeneratePropertiesFile(String inputFolderPath, String outputFolderPath, boolean force) throws IOException
	{
		File inputFolder = new File(inputFolderPath);
		if(!inputFolder.isDirectory() || !inputFolder.exists())
			throw new IllegalArgumentException("Please provide a valid and existing folder!");
		
		SortedMap<String, String> fileHeaders = new TreeMap<String, String>();
		SortedMap<String, Float> tempCN2MBPC = new TreeMap<String, Float>();
		
		// Process all charsetinfo files in the folder:
		for(File charsetFile : inputFolder.listFiles((FileFilter) new WildcardFileFilter("*." + CHARSETINFO_FILE_EXTENSION)))
		{
			try(UnicodeBOMInputStream input = new UnicodeBOMInputStream(new FileInputStream(charsetFile));
				BufferedReader reader = new BufferedReader(input.getReader(CHARSETINFO_FILE_CHARSET)))
			{
				String line;
				while((line = reader.readLine()) != null)
				{
					if(line.isEmpty())
						continue; // skip blank lines
					if(line.charAt(0) == COMMENT_LINE_MARKER)
					{
						if(!fileHeaders.containsKey(charsetFile.getName()))
							fileHeaders.put(charsetFile.getName(), line.substring(1).trim());
						continue; // skip comment lines
					}
					// Parse Charset info:
					CharsetInfo csInfo = CharsetInfo.Parse(line);
					// Store/replace max maxBytesPerChar value:
					if(csInfo.maxBytesPerChar > 0.0f && (!tempCN2MBPC.containsKey(csInfo.name) || tempCN2MBPC.get(csInfo.name).floatValue() < csInfo.maxBytesPerChar))
						tempCN2MBPC.put(csInfo.name, csInfo.maxBytesPerChar);
				}
				if(!fileHeaders.containsKey(charsetFile.getName()))
					fileHeaders.put(charsetFile.getName(), "");
			}
		}
		
		// Compare information loaded from charsetinfo files with that in the resource bundle:
		boolean different = force || CharsetMaxMaxBytesPerCharBundle == null || tempCN2MBPC.size() != getNumberOfKnownCharsets();
		if(!different)
			for(Map.Entry<String, Float> mapping : tempCN2MBPC.entrySet())
				try
				{
					if(!Float.valueOf(CharsetMaxMaxBytesPerCharBundle.getString(mapping.getKey())).equals(mapping.getValue())) // getString throws Exception if key is not found
						throw new Exception("maxBytesPerChar mismatch!");
				}
				catch(Exception e)
				{
					different = true;
					break;
				}
		
		// Get output file:
		File outputFolder = new File(new File(outputFolderPath), ClassHelpers.classPackageAsResourcePath(CharsetHelpers.class));
		FileHelpers.createDirectory(outputFolder);
		File outputFile = new File(outputFolder, CMMBPC_PROPERTIES_FILE_NAME + "." + PROPERTIES_FILE_EXTENSION);

		// If the information is different or there is no properties file yet...
		if(different || !outputFile.exists())
		{	// Write new properties file (in package-specific subfolder of the given output folder):
			try(FileOutputStream fos = new FileOutputStream(outputFile);
				BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(fos, PROPERTIES_FILE_CHARSET)))
			{
				// Header:
				writer.write(COMMENT_LINE_MARKER + " Generated on " + TimeUtils.getISOTimestamp(System.currentTimeMillis(), false) + " from input files:" + EOL);
				for(Map.Entry<String, String> fileAndHeader : fileHeaders.entrySet())
				{
					writer.write(COMMENT_LINE_MARKER + "\t- " + fileAndHeader.getKey() + ":" + EOL);
					writer.write(COMMENT_LINE_MARKER + "\t\t" + fileAndHeader.getValue() + EOL);
				}
				writer.write(EOL);
				// Body:
				for(Map.Entry<String, Float> mapping : tempCN2MBPC.entrySet())
					writer.write(mapping.getKey() + "=" + mapping.getValue().toString() + EOL);
			}
		}
		
		return different;
	}
	
	/**
	 * @author mstevens
	 */
	static private final class CharsetInfo
	{
		
		// STATICS --------------------------------------------------
		static final char SEPARATOR = '|';
		static final float UNKNOWN_MAX_BYTES_PER_CHAR = -1.0f;
		
		static public CharsetInfo Parse(String charInfoString)
		{
			final int sepPos = charInfoString.indexOf(SEPARATOR);
			return new CharsetInfo(charInfoString.substring(0, sepPos), Float.parseFloat(charInfoString.substring(sepPos + 1)));
		}
		
		// DYNAMICS -------------------------------------------------
		public final String name;
		public final float maxBytesPerChar;
		
		/**
		 * @param charset
		 */
		public CharsetInfo(Charset charset)
		{
			this.name = charset.name();
			float charsetmaxBytesPerChar = UNKNOWN_MAX_BYTES_PER_CHAR;
			try
			{
				charsetmaxBytesPerChar = charset.newEncoder().maxBytesPerChar();
			}
			catch(Exception ignore) {}
			this.maxBytesPerChar = charsetmaxBytesPerChar;
		}
		
		/**
		 * @param charsetName
		 * @param charsetMaxBytesPerChar
		 */
		private CharsetInfo(String charsetName, float charsetMaxBytesPerChar)
		{
			this.name = charsetName;
			this.maxBytesPerChar = charsetMaxBytesPerChar;
		}
		
		@Override
		public String toString()
		{
			return name + SEPARATOR + Float.toString(maxBytesPerChar);
		}
		
	}
	
}