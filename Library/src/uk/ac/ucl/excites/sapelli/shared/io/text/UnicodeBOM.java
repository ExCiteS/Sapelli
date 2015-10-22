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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Arrays;

import org.apache.commons.io.Charsets;

import uk.ac.ucl.excites.sapelli.shared.util.Objects;

/**
 * <p>
 * An instance of the <code>UnicodeBOM</code> class represents the <b>byte order mark (BOM)</b>
 * specific to a Unicode ("UTF-x") encoding. Instances hold a {@link Charset}, corresponding
 * to the specific encoding, and a byte array containing the bytes that make up the BOM.</p>
 * <p>
 * The class has static instances corresponding to each of the 5 Unicode BOMs defined in the relevant
 * <a href="http://www.unicode.org/unicode/faq/utf_bom.html">Unicode FAQ document</a>, as well as a 6th
 * instance, {@link #NO_BOM}, representing the absence of any (known/detectable) BOM.</p>
 * <p>
 * This class also provides static methods to deal with reading, detecting, and writing Unicode BOMs.</p> 
 * <p>
 * <b>Note 1:</b><br/>
 * 	Java's {@link OutputStreamWriter} annoyingly does not insert BOMs automatically. The static {@code GetWriter()}
 * 	methods in this class solve this problem.</p>
 * <p>
 * <b>Note 2:</b><br/> 
 * 	While Java's {@code InpuStreamReader} detects and skips the BOM on UTF-16 files it does not recognise the UTF-8 BOM.
 * 	This problem can be solved by wrapping the {@link InputStream} in a {@link UnicodeBOMInputStream} in order to detect
 * 	and skip the BOM (and thereby detect the used {@link Charset}).</p> 
 * 
 * @author mstevens
 * 
 * @see http://koti.mbnet.fi/akini/java/java_utf8_xml
 * @see http://koti.mbnet.fi/akini/java/unicodereader
 * @see http://bugs.java.com/bugdatabase/view_bug.do?bug_id=4508058
 * @see http://tripoverit.blogspot.be/2007/04/javas-utf-8-and-unicode-writing-is.html
 * @see http://www.rgagnon.com/javadetails/java-handle-utf8-file-with-bom.html
 * @see https://svn.codehaus.org/jtstand/jtstand/tags/jtstand-1.5.9/jtstand-editor/src/main/java/org/fife/io/UnicodeWriter.java
 * @see https://github.com/gpakosz/UnicodeBOMInputStream
 */
public final class UnicodeBOM
{

	// STATICS --------------------------------------------------------------------------
	static public final UnicodeBOM UTF8_BOM		= new UnicodeBOM(Charsets.UTF_8,				new byte[] { (byte) 0xEF, (byte) 0xBB, (byte) 0xBF });
	
	static public final UnicodeBOM UTF16LE_BOM	= new UnicodeBOM(Charsets.UTF_16LE,				new byte[] { (byte) 0xFF, (byte) 0xFE });
	
	static public final UnicodeBOM UTF16BE_BOM	= new UnicodeBOM(Charsets.UTF_16BE,				new byte[] { (byte) 0xFE, (byte) 0xFF });
	
	static public final UnicodeBOM UTF32LE_BOM	= new UnicodeBOM(Charset.forName("UTF-32LE"),	new byte[] { (byte) 0xFF, (byte) 0xFE, (byte) 0x00, (byte) 0x00 });
	
	static public final UnicodeBOM UTF32BE_BOM	= new UnicodeBOM(Charset.forName("UTF-32BE"),	new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0xFE, (byte) 0xFF });
	
	static public final UnicodeBOM NO_BOM		= new UnicodeBOM(null,							new byte[] { });
	
	static private final UnicodeBOM[] BOMS = { UTF8_BOM, UTF16LE_BOM, UTF16BE_BOM, UTF32LE_BOM, UTF32BE_BOM, NO_BOM };
	
	/**
	 * Returns a UnicodeBOM instance for the given (Unicode) character set.
	 * 
	 * @param charset
	 * @return the UnicodeBOM for the given Charset, or {@code null} no matching BOM was found
	 */
	static public UnicodeBOM GetBOM(Charset charset)
	{
		for(UnicodeBOM bom : BOMS)
			if(bom.charset == charset)
				return bom;
		return null;
	}
	
	/**
	 * Returns a UnicodeBOM instance for the (Unicode) character set with the given name.
	 * 
	 * @param charsetName
	 * @return the UnicodeBOM for the named charset, or {@code null} no matching BOM was found
	 * @throws IllegalCharsetNameException if the specified charset name is illegal.
	 * @throws UnsupportedCharsetException if the desired charset is not supported by this runtime.
	 */
	static public UnicodeBOM GetBOM(String charsetName) throws IllegalCharsetNameException, UnsupportedCharsetException
	{
		return GetBOM(Charset.forName(charsetName));
	}
	
	/**
	 * Returns the UnicodeBOM instance matching the given byte order mark bytes, or {@link #NO_BOM} if
	 * the given byte array does not matching any of the (known) Unicode byte order marks. 
	 * 
	 * @param bomBytes
	 * @return a UnicodeBOM instance matching, never {@code null} but possibly {@link #NO_BOM}
	 */
	static public UnicodeBOM GetBOM(byte[] bomBytes)
	{
		for(UnicodeBOM bom : BOMS)
			if(bomBytes.length >= bom.bytes.length && Arrays.equals(bom.bytes, Arrays.copyOf(bomBytes, bom.bytes.length)))
				return bom;
		return NO_BOM;
	}
	
	/**
	 * @return the maximum length (in number of bytes) of the known Unicode byte order marks
	 */
	static /*package*/ int GetMaxBOMLength()
	{
		int max = 0;
		for(UnicodeBOM bom : BOMS)
			if(bom.bytes.length > max)
				max = bom.bytes.length;
		return max;
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the system default {@link Charset}.
	 * The given {@link OutputStream} is assumed not to be appending.
	 * If there is a known UnicodeBOM for the charset then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param append
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out) throws IllegalArgumentException, NullPointerException, IOException
	{
		return GetWriter(out, Charset.defaultCharset(), false);
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the system default {@link Charset}.
	 * If there is a known UnicodeBOM for that charset and {@code append} is {@code false} then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param append
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out, boolean append) throws IllegalArgumentException, NullPointerException, IOException
	{
		return GetWriter(out, Charset.defaultCharset(), append);
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the named {@link Charset}.
	 * The given {@link OutputStream} is assumed not to be appending.
	 * If there is a known UnicodeBOM for the charset then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param charsetName
	 * @param append
	 * @return
	 * @throws IllegalCharsetNameException
	 * @throws UnsupportedCharsetException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out, String charsetName) throws IllegalCharsetNameException, UnsupportedCharsetException, IllegalArgumentException, NullPointerException, IOException
	{
		return GetWriter(out, charsetName, false);
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the named {@link Charset}.
	 * If there is a known UnicodeBOM for that charset and {@code append} is {@code false} then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param charsetName
	 * @param append
	 * @return
	 * @throws IllegalCharsetNameException
	 * @throws UnsupportedCharsetException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out, String charsetName, boolean append) throws IllegalCharsetNameException, UnsupportedCharsetException, IllegalArgumentException, NullPointerException, IOException
	{
		return GetWriter(out, Charset.forName(charsetName), append);
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the given {@link Charset}.
	 * The given {@link OutputStream} is assumed not to be appending.
	 * If there is a known UnicodeBOM for the charset then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param charset
	 * @param append
	 * @return
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out, Charset charset) throws NullPointerException, IOException
	{
		return GetWriter(out, charset, false);
	}
	
	/**
	 * Returns an {@link OutputStreamWriter} using the given {@link Charset}.
	 * If there is a known UnicodeBOM for that charset and {@code append} is {@code false} then the appropriate BOM bytes will be written to the output.
	 * 
	 * @param out the {@link OutputStream} to write to
	 * @param charset
	 * @param append
	 * @return
	 * @throws NullPointerException
	 * @throws IOException if an I/O error occurs upon writing the BOM
	 */
	static public OutputStreamWriter GetWriter(OutputStream out, Charset charset, boolean append) throws NullPointerException, IOException
	{
		// Write BOM if needed:
		UnicodeBOM bom = null;
		if(!append && ((bom = GetBOM(charset)) != null))
			bom.writeBytes(out);
		// Return writer:
		return new OutputStreamWriter(out, charset);
	}
	
	// DYNAMICS -------------------------------------------------------------------------
	private final Charset charset;
	private final byte[] bytes;
	
	/**
	 * @param charset may be {@code null} if and only if {@code bytes.length} is 0 (-> "NO_BOM")
	 * @param bytes should not be {@code null}, may have length 0 if and only if {@code charset} is {@code null} (-> "NO_BOM")
	 */
	private UnicodeBOM(Charset charset, byte[] bytes)
	{
		if(bytes == null || (bytes.length == 0 && charset != null) || (bytes.length > 0 && charset == null))
			throw new IllegalArgumentException("Invalid charset and/or bytes");
		this.charset = charset;
		this.bytes = bytes;
	}
	
	public String getCharsetName()
	{
		if(charset != null)
			return charset.name();
		else
			return "NONE";
	}
	
	public Charset getCharset()
	{
		return charset;
	}
	
	public byte[] getBytes()
	{
		return Arrays.copyOf(bytes, bytes.length);
	}
	
	public int getLength()
	{
		return bytes.length;
	}
	
	public void writeBytes(OutputStream outputStream) throws IOException
	{
		outputStream.write(bytes);
	}
	
	@Override
	public String toString()
	{
		return getClass().getSimpleName() + '_' + getCharsetName();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof UnicodeBOM)
		{
			UnicodeBOM that = (UnicodeBOM) obj;
			return	Objects.equals(this.charset, that.charset) &&
					Arrays.equals(this.bytes, that.bytes);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (charset == null ? 0 : charset.hashCode());
		hash = 31 * hash + Arrays.hashCode(bytes);
		return hash;
	}

}
