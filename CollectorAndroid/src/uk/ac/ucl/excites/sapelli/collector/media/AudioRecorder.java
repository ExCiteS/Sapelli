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

package uk.ac.ucl.excites.sapelli.collector.media;

import java.io.File;

import android.media.MediaRecorder;
import android.util.Log;


/**
 * A Recording Utility Class
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
@SuppressWarnings("unused")
public class AudioRecorder
{
	
	static private final String TAG = "AudioRecorder";
	
	private MediaRecorder mediaRecorder;
	private File audioFile;
	private boolean isRecording;

	/**
	 * Sets up the audio recorder to produce a file with the given filename in the given folder
	 * 
	 * @param dataFolder
	 * @param filename
	 */
	public AudioRecorder(File audioFile)
	{
		this.audioFile = audioFile;
		isRecording = false;
	}

	/**
	 * Starts a new recording.
	 */
	public void start() throws Exception
	{
		mediaRecorder = new MediaRecorder();
		mediaRecorder.setAudioSource(MediaRecorder.AudioSource.MIC);
		mediaRecorder.setOutputFormat(MediaRecorder.OutputFormat.THREE_GPP); //TODO make configurable
		mediaRecorder.setAudioEncoder(MediaRecorder.AudioEncoder.AMR_NB); //TODO make configurable
		mediaRecorder.setOutputFile(audioFile.getAbsolutePath());
		mediaRecorder.prepare();
		mediaRecorder.start();
		isRecording = true;
		//Log.d(TAG, "Started recording audio (output file: " + audioFile.getAbsolutePath() + ").");
	}

	/**
	 * Stops a recording that has been previously started.
	 * 
	 * @see http://stackoverflow.com/a/11984387/1084488
	 */
	public void stop() throws Exception
	{
		if (isRecording)
		{
			mediaRecorder.stop();
			mediaRecorder.reset();
			mediaRecorder.release();
			mediaRecorder = null;
			//Log.d(TAG, "Stopped recording audio (output file: " + audioFile.getAbsolutePath() + ").");
		}
	}
	
}