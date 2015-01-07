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
 * @author mstevens, benelliott
 *
 */
public class TTVSynthesisTask implements PostProcessTask
{
	
	private final String language;
	private final String textToSynthesise;
	private final String audioFileRelativePath;

	/**
	 * @param textToSynthesise
	 * @param audioFileRelativePath
	 */
	public TTVSynthesisTask(String textToSynthesise, String audioFileRelativePath, String language)
	{
		this.textToSynthesise = textToSynthesise;
		this.audioFileRelativePath = audioFileRelativePath;
		this.language = language;
	}

	/**
	 * @return the textToSynthesise
	 */
	public String getTextToSynthesise()
	{
		return textToSynthesise;
	}

	/**
	 * @return the audioFileRelativePath
	 */
	public String getAudioFileRelativePath()
	{
		return audioFileRelativePath;
	}
	
	/**
	 * @return the language in which the text should be synthesised, as a BCP 47 format string (e.g. en-GB)
	 */
	public String getLanguage() {
		return language;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessTask#execute(uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor, uk.ac.ucl.excites.sapelli.collector.model.Project, uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper)
	 */
	@Override
	public void execute(PostProcessor executor, Project project, WarningKeeper warningKeeper) throws Exception
	{
		executor.execute(this, project, warningKeeper);
	}

}
