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

/**
 * @author mstevens
 *
 */
public final class ExceptionHelpers
{

	private ExceptionHelpers() {}
	
	static public String getMessage(Throwable e)
	{
		if(e != null)
		{
			String msg = e.getLocalizedMessage(); 
			return msg != null && !msg.isEmpty() ? e.getLocalizedMessage() : e.toString();
		}
		else
			return null;	
	}
	
	static public String getMessageAndCause(Throwable e)
	{
		if(e != null)
			return getMessage(e) + (e.getCause() != null ? " (" + getMessage(e.getCause()) + ")" : "");
		else
			return null;
	}

}
