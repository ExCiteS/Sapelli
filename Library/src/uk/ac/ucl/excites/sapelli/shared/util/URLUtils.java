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

/**
 * @author mstevens
 *
 */
public final class URLUtils
{
	
	static public final String PROTOCOL_HTTP = "http://";
	
	static public final String PROTOCOL_HTTPS = "https://";

	private URLUtils() {}
	
	static public String stripHTTP(String url)
	{
		return url.replaceFirst("^(http://|https://)", "");
	}
	
	static public String addTrailingSlash(String url)
	{
		return url + (url.endsWith("/") ? "" : "/");
	}
	
	static public String stripTrailingSlash(String url)
	{
		return url.endsWith("/") ? url.substring(0, url.length() - 1) : url;
	}
	
}
