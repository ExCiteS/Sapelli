package uk.ac.ucl.excites.sapelli.shared.util;

import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;

/**
 * Help for dealing with Unicode i/o.
 * For now it only provides byte order markers (BOMs) for UTF-x encodings.
 * 
 * Java's standard OutputStreamWriter annoyingly does not insert BOMs automatically.
 * Moreover, while InpuStreamReader does detect and skips the BOM on UTF-16 files it
 * does not recognise the UTF-8 BOM.
 * 
 * @author mstevens
 * 
 * @see http://koti.mbnet.fi/akini/java/java_utf8_xml
 * @see http://koti.mbnet.fi/akini/java/unicodereader
 * @see http://bugs.java.com/bugdatabase/view_bug.do?bug_id=4508058
 * @see http://tripoverit.blogspot.be/2007/04/javas-utf-8-and-unicode-writing-is.html
 * @see http://www.rgagnon.com/javadetails/java-handle-utf8-file-with-bom.html
 * @see https://svn.codehaus.org/jtstand/jtstand/tags/jtstand-1.5.9/jtstand-editor/src/main/java/org/fife/io/UnicodeWriter.java
 */
public final class UnicodeHelpers
{

	public static final Charset UTF8 = Charset.forName("UTF-8");
	
	public static final Charset UTF16LE = Charset.forName("UTF-16LE");
	
	public static final Charset UTF16BE = Charset.forName("UTF-16BE");
	
	public static final Charset UTF32LE = Charset.forName("UTF-32LE");
	
	public static final Charset UTF32BE = Charset.forName("UTF-32BE");
	
	public static final byte[] UTF8_BOM = new byte[] { (byte) 0xEF, (byte) 0xBB, (byte) 0xBF };

	public static final byte[] UTF16LE_BOM = new byte[] { (byte) 0xFF, (byte) 0xFE };

	public static final byte[] UTF16BE_BOM = new byte[] { (byte) 0xFE, (byte) 0xFF };

	public static final byte[] UTF32LE_BOM = new byte[] { (byte) 0xFF, (byte) 0xFE, (byte) 0x00, (byte) 0x00 };

	public static final byte[] UTF32BE_BOM = new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0xFE, (byte) 0xFF };
	
	/**
	 * Returns a byte order marker (BOM) for a specified character set.
	 * 
	 * @param charsetName
	 * @return the BOM for the named charset, or null no matching BOM was found
	 * @throws IllegalCharsetNameException if the specified charset name is illegal.
	 * @throws UnsupportedCharsetException if the desired charset is not supported by this runtime.
	 */
	public static byte[] getBom(String charsetName) throws IllegalCharsetNameException, UnsupportedCharsetException
	{
		return getBom(Charset.forName(charsetName));
	}
	
	/**
	 * Returns a byte order marker (BOM) for a specified character set.
	 * 
	 * @param charset
	 * @return the BOM for the given charset, or null no matching BOM was found
	 */
	public static byte[] getBom(Charset charset)
	{
		if(UTF8.equals(charset))
			return UTF8_BOM;
		if(UTF16LE.equals(charset))
			return UTF16LE_BOM;
		if(UTF16BE.equals(charset))
			return UTF16BE_BOM;
		if(UTF32LE.equals(charset))
			return UTF32LE_BOM;
		if(UTF32BE.equals(charset))
			return UTF32BE_BOM;
		else
			return null;
	}

	private UnicodeHelpers() { /* should never be instantiated */ }

}
