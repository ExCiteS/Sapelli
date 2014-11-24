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

package uk.ac.ucl.excites.sapelli.collector.load.process;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;


/**
 * Interface for post-loading task execution.
 * Add an execute() method here for each new class which implements {@link PostProcessTask}.
 * 
 * @author mstevens
 */
public interface PostProcessor
{

	/**
	 * To be called before the first task execution for a given Project
	 *  
	 * @param project
	 */
	public void initialise(Project project);
	
	/**
	 * @param ttsTask
	 * @param project
	 * @param warningKeeper
	 * @throws Exception
	 */
	public void execute(TTSSynthesisTask ttsTask, Project project, WarningKeeper warningKeeper) throws Exception;
	
	/**
	 * Free any resources used by the post-processor.
	 */
	public void freeResources();
}
