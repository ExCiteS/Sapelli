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

package uk.ac.ucl.excites.sapelli.collector.db.exceptions;

import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * @author mstevens
 *
 */
public class ProjectSignatureClashException extends ProjectDuplicateException
{

	private static final long serialVersionUID = 2L;

	public ProjectSignatureClashException(Project storedProject)
	{
		super("There is a previously loaded project which is different but has the same signature \"" + storedProject.toString(false) + "\". Either delete the existing one or change the version of the new one.");
	}

}
