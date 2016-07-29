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

package uk.ac.ucl.excites.sapelli.storage.util;

/**
 * @author mstevens
 */
public class UnexportableRecordsException extends IllegalArgumentException
{

	private static final long serialVersionUID = 2L;
	
	public UnexportableRecordsException(int count)
	{
		super("Could not export " + count + " record" + (count > 1 ? "s" : "") + " because " + (count > 1 ? "their" : "its") + " schema is unexportable.");
	}

}
