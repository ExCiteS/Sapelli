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

package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.Comparator;

/**
 * Comparator for software version strings
 * 
 * E.g.:
 * 	- 1.0 > 2.0
 *  - 1.1.3 < 1.2.1
 *  - 3.12 > 3.3.1
 *  - 2.0 > 2.0 beta > 2.0 alpha
 *  - 2.11 b1 < 2.11 b5
 * 
 * @see Code adapted from <a href="http://stackoverflow.com/a/10034633/1084488">http://stackoverflow.com/a/10034633/1084488</a>
 * @author mstevens
 */
public class VersionComparator implements Comparator<String>
{

	static public boolean isAtLeast(String testVersion, String minimumVersion)
	{
		return new VersionComparator().compare(testVersion, minimumVersion) >= 0;
	}
	
	public boolean equals(Object o1, Object o2)
	{
		if(o1 instanceof String && o2 instanceof String)
			return compare((String) o1, (String) o2) == 0;
		else
			return o1 == o2;
	}

	public int compare(String version1, String version2)
	{
		if(version1 == null)
			return version2 == null ? 0 : Integer.MIN_VALUE;
		if(version2 == null)
			return Integer.MAX_VALUE;
		
		VersionTokenizer tokenizer1 = new VersionTokenizer(version1);
		VersionTokenizer tokenizer2 = new VersionTokenizer(version2);

		int number1 = 0, number2 = 0;
		String suffix1 = "", suffix2 = "";

		while(tokenizer1.moveNext())
		{
			if(!tokenizer2.moveNext())
			{
				do
				{
					number1 = tokenizer1.getNumber();
					suffix1 = tokenizer1.getSuffix();
					if(number1 != 0 || suffix1.length() != 0)
					{	// Version one is longer than number two, and non-zero
						return 1;
					}
				}
				while(tokenizer1.moveNext());
				// Version one is longer than version two, but zero
				return 0;
			}

			number1 = tokenizer1.getNumber();
			suffix1 = tokenizer1.getSuffix();
			number2 = tokenizer2.getNumber();
			suffix2 = tokenizer2.getSuffix();

			if(number1 < number2)
			{	// Number one is less than number two
				return -1;
			}
			if(number1 > number2)
			{	// Number one is greater than number two
				return 1;
			}

			boolean empty1 = suffix1.length() == 0;
			boolean empty2 = suffix2.length() == 0;

			if(empty1 && empty2)
				continue; // No suffixes
			if(empty1)
				return 1; // First suffix is empty (1.2 > 1.2b)
			if(empty2)
				return -1; // Second suffix is empty (1.2a < 1.2)

			// Lexical comparison of suffixes
			int result = suffix1.compareTo(suffix2);
			if(result != 0)
				return result;
		}
		if(tokenizer2.moveNext())
		{
			do
			{
				number2 = tokenizer2.getNumber();
				suffix2 = tokenizer2.getSuffix();
				if(number2 != 0 || suffix2.length() != 0)
				{
					// Version one is longer than version two, and non-zero
					return -1;
				}
			}
			while(tokenizer2.moveNext());

			// Version two is longer than version one, but zero
			return 0;
		}
		return 0;
	}

	public class VersionTokenizer
	{
		private final String versionString;
		private final int length;

		private int position;
		private int number;
		private String suffix;

		public VersionTokenizer(String versionString)
		{
			if(versionString == null)
				throw new IllegalArgumentException("versionString is null");
			this.versionString = versionString;
			this.length = versionString.length();
		}
		
		public int getNumber()
		{
			return number;
		}

		public String getSuffix()
		{
			return suffix;
		}

		public boolean moveNext()
		{
			number = 0;
			suffix = "";

			// No more characters
			if(position >= length)
				return false;

			while(position < length)
			{
				char c = versionString.charAt(position);
				if(c < '0' || c > '9')
					break;
				number = number * 10 + (c - '0');
				position++;
			}

			int suffixStart = position;
			while(position < length)
			{
				char c = versionString.charAt(position);
				if(c == '.')
					break;
				position++;
			}

			suffix = versionString.substring(suffixStart, position);
			if(position < length)
				position++;

			return true;
		}
	}

}