package uk.ac.ucl.excites.collector.util;

import java.io.File;
import java.io.IOException;

import android.media.MediaRecorder;
import android.util.Log;

/**
 * A Recording Utility Class
 * 
 * @author Michalis Vitos
 * 
 */
public class AudioRecorder
{

	final private MediaRecorder mMediaRecorder = new MediaRecorder();
	final private String path;
	final private String filename;
	final private String extension = ".3gp";
	private boolean isRecording;

	/**
	 * Creates a new audio recording at the given path (relative to root of SD
	 * card).
	 * 
	 * @param path
	 * @param filename
	 */
	public AudioRecorder(String path, String filename)
	{
		this.filename = filename;
		this.path = sanitizePath(path, filename);
		isRecording = false;
		
		if (Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "-- Filepath for the recordings: " + this.path );
	}

	private String sanitizePath(String path, String filename)
	{
		if (!path.startsWith("/"))
		{
			path = "/" + path;
		} else if (!path.endsWith("/"))
		{
			path = path + "/";
		} 
		
		path = path + filename;

		if (!path.contains("."))
		{
			path += extension;
		}
		return path;
	}

	/**
	 * Starts a new recording.
	 */
	public void start() throws IOException
	{
		String state = android.os.Environment.getExternalStorageState();
		if (!state.equals(android.os.Environment.MEDIA_MOUNTED))
		{
			throw new IOException("SD Card is not mounted.  It is " + state + ".");
		}

		// make sure the directory we plan to store the recording exists
		File directory = new File(path).getParentFile();
		if (!directory.exists() && !directory.mkdirs())
		{
			throw new IOException("Path to file could not be created.");
		}

		mMediaRecorder.setAudioSource(MediaRecorder.AudioSource.MIC);
		mMediaRecorder.setOutputFormat(MediaRecorder.OutputFormat.THREE_GPP);
		mMediaRecorder.setAudioEncoder(MediaRecorder.AudioEncoder.AMR_NB);
		mMediaRecorder.setOutputFile(path);
		mMediaRecorder.prepare();
		mMediaRecorder.start();
		isRecording = true;
	}

	/**
	 * Stops a recording that has been previously started.
	 */
	public void stop() throws IOException
	{
		if (isRecording)
		{
			mMediaRecorder.stop();
			mMediaRecorder.release();
		}
	}

	public String getPath()
	{
		return path;
	}

	public String getFilename()
	{
		return filename + extension;
	}
}