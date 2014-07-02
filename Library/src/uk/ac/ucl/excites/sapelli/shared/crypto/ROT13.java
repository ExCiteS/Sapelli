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

package uk.ac.ucl.excites.sapelli.shared.crypto;

/**
 * ROT13 and "NumROT5" coder
 * 
 * @author mstevens
 * 
 */
public final class ROT13
{

	private ROT13() {}

	public static String rot13(String text)
	{
		StringBuilder bld = new StringBuilder(text.length());
		for(char c : text.toCharArray())
			bld.append(rot13(c));
		return bld.toString();
	}
	
	public static String rot13NumRot5(String text)
	{
		StringBuilder bld = new StringBuilder(text.length());
		for(char c : text.toCharArray())
			bld.append(numRot5(rot13(c)));
		return bld.toString();
	}
	
	private static char rot13(char c)
	{
		if((c >= 'A') && (c <= 'Z'))
			c = (char) ((((c - 'A') + 13) % 26) + 'A');
		if((c >= 'a') && (c <= 'z'))
			c = (char) ((((c - 'a') + 13) % 26) + 'a');
		return c;
	}
	
	private static char numRot5(char c)
	{
		if((c >= '0') && (c <= '9'))
			c = (char) ((((c - '0') + 5) % 10) + '0');
		return c;
	}

}
