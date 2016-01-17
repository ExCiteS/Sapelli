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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.apache.commons.io.Charsets;
import org.apache.commons.lang3.ArrayUtils;

/**
 * @author mstevens
 *
 */
public final class StringUtils
{

	private StringUtils() {}

	static public String nullToEmpty(String str)
	{
		return str == null ? "" : str;
	}
	
	static public String emptyToNull(String str)
	{
		return str != null && str.isEmpty() ? null : str;
	}
	
	/**
	 * Returns the number bytes a given string takes up when encoded with a given charset 
	 * 
	 * @param string
	 * @return number of bytes that would be used to write the string
	 */
	static public int sizeBytes(String string, Charset charset)
	{
		return string.getBytes(charset).length;
	}
	
	/**
	 * Returns the number bytes a given string takes up when encoded as UTF-8 
	 * 
	 * @param string
	 * @return number of bytes that would be used to write the string
	 */
	static public int sizeBytes(String string)
	{
		return sizeBytes(string, Charsets.UTF_8);
	}
	
	static public String addVariableFrontPadding(String str, int desiredLength, char padding)
	{
		if(str == null)
			return str;
		StringBuffer bff = new StringBuffer();
		for(int i = str.length(); i < desiredLength; i++)
			bff.append(padding);
		bff.append(str);
		return bff.toString();
	}
	
	static public String addFixedFrontPadding(String str, int paddingLength, char padding)
	{
		StringBuffer bff = new StringBuffer();
		for(int i = 0; i < paddingLength; i++)
			bff.append(padding);
		bff.append(str);
		return bff.toString();
	}
	
	static public String addTabsFront(String str, int tabs)
	{
		return addFixedFrontPadding(str, tabs, '\t');
	}
	
	static public String join(String[] parts, String separator)
	{
		return join(Arrays.asList(parts), separator);
	}
	
	static public String join(List<? extends Object> parts, String separator)
	{
		if(parts == null)
			return null;
		if(parts.isEmpty())
			return "";
		Iterator<?> i = parts.iterator();
		StringBuffer bff = new StringBuffer(i.next().toString());
		while(i.hasNext())
		{
			bff.append(separator);
			bff.append(i.next().toString());
		}
		return bff.toString();
	}
	
	static public String replaceWhitespace(String str, String replacement)
	{
		return str.replaceAll("\\s+", replacement);
	}
	
	/**
	 * @param subject
	 * @param needle
	 * @param values
	 * @return
	 */
	static public String replaceWithValues(String subject, String needle, String[] values)
	{
		return replaceWithValues(subject, needle, Arrays.copyOf(values, values.length, Object[].class));
	}
	
	/**
	 * @param subject
	 * @param needle
	 * @param values
	 * @return
	 * 
	 * @see http://stackoverflow.com/a/16975971/1084488
	 */
	static public String replaceWithValues(String subject, String needle, Object[] values)
	{
		return String.format(subject.replace("%", "%%").replace(needle, "%s"), values);
	}
	
	/**
	 * Simple escaping method to clear Strings of a single to-be-avoided char
	 * 
	 * @param str may be null, in which case null will be returned
	 * @param avoid
	 * @param replacement
	 * @param prefix
	 * @return
	 */
	static public String escape(String str, char avoid, char replacement, char prefix)
	{
		if(str == null || "".equals(str))
			return str;
		StringBuilder bldr = new StringBuilder();
		for(char c : str.toCharArray())
			if(c == avoid)
			{	// avoid --> prefix + replacement
				bldr.append(prefix);
				bldr.append(replacement);
			}
			else
			{
				bldr.append(c);
				if(c == prefix)
					bldr.append(prefix); // prefix --> prefix + prefix
			}
		return bldr.toString();
	}
	
	/**
	 * Simple de-escaping method which reverses the work of {@link #escape(String, char, char, char)}
	 * 
	 * @param str may be null, in which case null will be returned
	 * @param avoid
	 * @param replacement
	 * @param prefix
	 * @return
	 */
	static public String deescape(String str, char avoid, char replacement, char prefix)
	{
		if(str == null)
			return null;
		StringBuilder bldr = new StringBuilder();
		boolean prevPrefix = false;
		for(char c : str.toCharArray())
		{
			if(prevPrefix)
			{
				if(c == replacement)
					bldr.append(avoid); // prefix + replacement --> avoid
				else
					bldr.append(prefix); // prefix + prefix --> prefix
				prevPrefix = false;
			}
			else
			{
				if(c == prefix)
					prevPrefix = true;
				else
					bldr.append(c);
			}				
		}
		return bldr.toString();
	}
	
	/**
	 * Escapes occurrences of the given {@code avoid} characters and the given {@code wrapDelimiter} in
	 * the given {@code str}ing by wrapping the {@code str}ing in {@code wrapDelimiter} and doubling
	 * existing occurrences of the {@code wrapDelimiter}.
	 * 
	 * @param str
	 * @param wrapDelimiter
	 * @param forceWrapping
	 * @param avoid - maybe empty, in which case only occurrences of wrapDelimiter itself will cause unforced wrapping
	 * @return
	 * 
	 * @see #deescapeByDoublingAndWrapping(String, char)
	 */
	static public String escapeByDoublingAndWrapping(final String str, final char wrapDelimiter, final boolean forceWrapping, char... avoid)
	{
		if(str == null)
			return str;
		if(avoid == null)
			avoid = new char[0];
		boolean needsWrapping = forceWrapping;
		StringBuilder bldr = new StringBuilder(str.length());
		for(char c : str.toCharArray())
		{
			if(c == wrapDelimiter)
				bldr.append(wrapDelimiter); // escape wrapDelimiter occurrences by doubling them
			if(c == wrapDelimiter || ArrayUtils.contains(avoid, c))
				needsWrapping = true;
			bldr.append(c);
		}			
		return (needsWrapping ? wrapDelimiter : "") + bldr.toString() + (needsWrapping ? wrapDelimiter : "");
	}
	
	/**
	 * If the given {@code str}ing starts and ends with the given {@code wrapDelimiter} then
	 * these are removed and doubled occurrences in the wrapped contents are replaced by single ones.
	 * 
	 * @param str
	 * @param wrapDelimiter
	 * @return
	 * 
	 * @see #escapeByDoublingAndWrapping(String, char[], char, boolean)
	 */
	static public String deescapeByDoublingAndWrapping(final String str, final char wrapDelimiter)
	{
		if(str == null || str.length() < 2)
			return str;
		boolean wrapped = (str.charAt(0) == wrapDelimiter) && (str.charAt(str.length() - 1) == wrapDelimiter);
		// Remove outer wrapDelimiters and replace inner doubled wrapDelimiters by single ones:
		return str.substring(wrapped ? 1 : 0, str.length() - (wrapped ? 1 : 0)).replace("" + wrapDelimiter + wrapDelimiter, "" + wrapDelimiter); 
	}
	
	static public int countOccurances(String haystack, char needle)
	{
		int count = 0;
		for(char c : haystack.toCharArray())
			if(c == needle)
				count++;
		return count;
	}
	
	static public String capitalizeFirstLetter(String original)
	{
		if(original.length() == 0)
			return original;
		return original.substring(0, 1).toUpperCase(Locale.getDefault()) + original.substring(1);
	}
	
}
