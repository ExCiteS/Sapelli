/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.Charsets;
import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.sapelli.shared.util.ClassHelpers;

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
 * The {@link #GetMaxBytesPerChar(Charset)} method provides the maximum {@code maxBytesPerChar} value, across
 * several Java VM/runtime implementations/versions, for the a given {@code Charset}. The {@code maxBytesPerChar}
 * values for different {@code Charset}s as implemented on different VMs/runtimes are loaded from resource files
 * placed in the "charsetinfo" package (which must be a subpackage of the package this class resides in).</p>
 * <p>
 * The {@link #WriteCharsetInfo(File, String)} method writes out a "charsetinfo" file with the {@code maxBytesPerChar}
 * values for all {@code Charset}s supported by the current platform/device.</p>
 * 
 * @author mstevens
 */
public final class CharsetHelpers
{
	
	private CharsetHelpers() {}
	
	private static final Charset DEFAULT_CHARSETINFO_CHARSET = Charsets.UTF_8;
	
	private static final char CHARINFO_FILE_COMMENT_LINE_MARKER = '#';
	
	private static final Map<String, Float> CharsetName2MaxBytesPerChar;
	
	static
	{	// Initialise CharsetName2MaxBytesPerChar:
		Map<String, Float> tempCN2MBPC = new HashMap<String, Float>();
		Class<CharsetHelpers> clazz = CharsetHelpers.class;
		try
		{
			String charsetInfoPath = ClassHelpers.classPackageAsResourcePath(clazz) + "/charsetinfo";
			for(String charsetInfoFile : IOUtils.readLines(clazz.getClassLoader().getResourceAsStream(charsetInfoPath), Charsets.UTF_8))
			{
				// Read charsetinfo file:
				try(UnicodeBOMInputStream input = new UnicodeBOMInputStream(clazz.getResourceAsStream("/" + charsetInfoPath + "/" + charsetInfoFile));
					BufferedReader reader = new BufferedReader(input.getReader(DEFAULT_CHARSETINFO_CHARSET)))
				{
					String line;
					while((line = reader.readLine()) != null)
					{
						if(line.isEmpty() || line.charAt(0) == CHARINFO_FILE_COMMENT_LINE_MARKER)
							continue; // skip blank & comment lines
						// Parse Charset info:
						CharsetInfo csInfo = CharsetInfo.Parse(line);
						// Store/replace max maxBytesPerChar value:
						if(csInfo.maxBytesPerChar > 0.0f && (!tempCN2MBPC.containsKey(csInfo.name) || tempCN2MBPC.get(csInfo.name).floatValue() < csInfo.maxBytesPerChar))
							tempCN2MBPC.put(csInfo.name, csInfo.maxBytesPerChar);
					}
				}
			}
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}
		CharsetName2MaxBytesPerChar = tempCN2MBPC; // TODO if we ever start using Guava we should use an ImmutableMap here (which isn't the same as Java's unmodifyable map)
	}
	
	/**
	 * Returns the maximum "maxBytesPerChar" value for the given {@link Charset}
	 * across all Java VM/runtime implementations/versions we know about.
	 * 
	 * @param charset
	 * @return
	 * @throws IllegalArgumentException
	 */
	static public final float GetMaxBytesPerChar(Charset charset) throws IllegalArgumentException
	{
		float maxBytesPerChar;
		if(CharsetName2MaxBytesPerChar.containsKey(charset.name()))
			// Consult map:
			maxBytesPerChar = CharsetName2MaxBytesPerChar.get(charset.name());
		else
			// Fall-back (determine now):
			maxBytesPerChar = new CharsetInfo(charset).maxBytesPerChar;
		// Check:
		if(maxBytesPerChar <= 0.0f)
			throw new IllegalArgumentException("maxBytesPerChar for Charset \"" + charset.name() + "\" is indeterminable.");
		//else:
			return maxBytesPerChar;
	}
	
	/**
	 * Writes a "charsetinfo" file containing information about all supported {@link Charset}s on the current machine/platform.
	 * 
	 * @param outputFile
	 * @throws IOException
	 */
	static public final void WriteCharsetInfo(File outputFile, String header) throws IOException
	{
		try(FileOutputStream fos = new FileOutputStream(outputFile);
			BufferedWriter writer = new BufferedWriter(UnicodeBOM.GetWriter(fos, DEFAULT_CHARSETINFO_CHARSET)))
		{
			if(header != null)
			{
				if(!header.isEmpty())
					writer.write(CHARINFO_FILE_COMMENT_LINE_MARKER + " " + header.trim());
				writer.newLine();
			}
			for(Charset cs : Charset.availableCharsets().values())
			{
				writer.write(new CharsetInfo(cs).toString());
				writer.newLine();
			}
		}
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
		 * @param maxBytesPerChar
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