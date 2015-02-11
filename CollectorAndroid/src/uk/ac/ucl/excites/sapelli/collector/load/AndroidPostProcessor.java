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
import uk.ac.ucl.excites.sapelli.collector.load.process.TTVSynthesisTask;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper;
import android.content.Context;

/**
 * @author mstevens, benelliott
 *
 */
public class AndroidPostProcessor implements PostProcessor
{
	
	@SuppressWarnings("unused")
    private static final String TAG = "AndroidPostProcessor";
	
	private Context context;
	private FileStorageProvider fileStorageProvider;
	private TextToVoice ttv;
	private boolean warnedAboutLanguage = false;

	public AndroidPostProcessor(Context context, FileStorageProvider fileStorageProvider)
	{
		this.context = context;
		this.fileStorageProvider = fileStorageProvider;
	}
	
	@Override
    public void initialise(Project project)
	{
		// Reset "already warned" flags:
		warnedAboutLanguage = false;
    }
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.process.PostProcessor#execute(uk.ac.ucl.excites.sapelli.collector.load.process.TTSSynthesisTask, uk.ac.ucl.excites.sapelli.collector.model.Project, uk.ac.ucl.excites.sapelli.shared.util.WarningKeeper)
	 */
	@Override
	public void execute(TTVSynthesisTask ttsTask, Project project, WarningKeeper warningKeeper) throws Exception
	{
		if (ttv == null)
			ttv = new TextToVoice(context);
		
		String filepath = (fileStorageProvider.getProjectSoundFile(project, ttsTask.getAudioFileRelativePath())).getAbsolutePath();
		
		// keep track of whether or not the user has been warned about an invalid language code (only want to warn them once):
	    try
	    {
	    	ttv.processSpeechToFile(ttsTask.getTextToSynthesise(), filepath, ttsTask.getLanguage());
	    }
	    catch (TTVSynthesisFailedException e)
	    {
			warningKeeper.addWarning("Text-to-speech synthesis: Error when trying to synthesise text -- \""+e.getText()+"\". This synthesis job will be skipped and nothing will be played when the project is run.");
	    }
	    catch (TTVUnsupportedLanguageException e)
	    {
	    	if (!warnedAboutLanguage)
	    	{
	    		warningKeeper.addWarning("Text-to-speech synthesis: specified language code not supported for synthesis. Make sure the provided language code is valid, but know that Android will not support all valid language codes for speech synthesis.");
	    		warnedAboutLanguage = true;
	    	}
	    }
	}

	/**
	 * Frees any resources that were being used by the PostProcessor
	 */
	@Override
	public void freeResources()
	{
		// destroy TTS engine:
		if(ttv != null)
		{
			ttv.destroy();
			ttv = null;
		}
	}

}
