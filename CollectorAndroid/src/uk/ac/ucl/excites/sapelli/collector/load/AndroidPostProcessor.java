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

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor;
import uk.ac.ucl.excites.sapelli.collector.load.process.TTSSynthesisTask;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.media.TTVFailedException;
import uk.ac.ucl.excites.sapelli.collector.util.TextToVoice;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;
import android.content.Context;

/**
 * @author mstevens, benelliott
 *
 */
public class AndroidPostProcessor implements PostProcessor
{
	
	private static final String TAG = "AndroidPostProcessor";
	private Context context;
	private FileStorageProvider fileStorageProvider;
	private TextToVoice ttv;

	public AndroidPostProcessor(Context context, FileStorageProvider fileStorageProvider) {
		this.context = context;
		this.fileStorageProvider = fileStorageProvider;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor#execute(uk.ac.ucl.excites.sapelli.collector.load.process.TTSSynthesisTask, uk.ac.ucl.excites.sapelli.collector.model.Project, uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper)
	 */
	@Override
	public void execute(TTSSynthesisTask ttsTask, Project project, WarningKeeper warningKeeper) throws Exception
	{
		if (ttv == null)
			ttv = new TextToVoice(context);
		
		String filepath = (fileStorageProvider.getProjectSoundFile(project, ttsTask.getAudioFileRelativePath())).getAbsolutePath();
		
		try {
	        ttv.processSpeechToFile(ttsTask.getTextToSynthesise(), filepath); //TODO deprecated as of Lollipop
        } catch (TTVFailedException e) {
	        warningKeeper.addWarning("Unable to synthesise text to speech: \""+e.getText()+"\". Skipping this text without creating an audio file.");
        }
	}

	/**
	 * Destroys any resources that were being used by the PostProcessor
	 */
	public void destroy() {	
		// destroy TTS engine:
		if (ttv != null) {
			ttv.destroy();
			ttv = null;
		}
	}
	
}
