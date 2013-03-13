package uk.ac.ucl.excites.collector.media;

import java.io.File;

import android.media.MediaRecorder;
import android.util.Log;


/**
 * A Recording Utility Class
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
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
	public AudioRecorder(File dataFolder, String filename)
	{
		this.audioFile = new File(dataFolder.getAbsolutePath() + File.separatorChar + filename);
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
		Log.d(TAG, "Started recording audio (output file: " + audioFile.getAbsolutePath() + ").");
	}

	/**
	 * Stops a recording that has been previously started.
	 */
	public void stop() throws Exception
	{
		if (isRecording)
		{
			mediaRecorder.stop();
			mediaRecorder.release();
			mediaRecorder = null;
			Log.d(TAG, "Stopped recording audio (output file: " + audioFile.getAbsolutePath() + ").");
		}
	}
	
}