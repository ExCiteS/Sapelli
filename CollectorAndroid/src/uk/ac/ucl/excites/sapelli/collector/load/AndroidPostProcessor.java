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

package uk.ac.ucl.excites.sapelli.collector.load;

import uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor;
import uk.ac.ucl.excites.sapelli.collector.load.process.TTSSynthesisTask;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;

/**
 * @author mstevens
 *
 */
public class AndroidPostProcessor implements PostProcessor
{
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor#execute(uk.ac.ucl.excites.sapelli.collector.load.process.TTSSynthesisTask, uk.ac.ucl.excites.sapelli.collector.model.Project, uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper)
	 */
	@Override
	public void execute(TTSSynthesisTask ttsTask, Project project, WarningKeeper warningKeeper) throws Exception
	{
		// TODO implement TTS support here, avoid throwing exceptions unless really fatal, for everything else (including failure to produce mp3) use warningKeeper.addWarning(String)
	}

}
