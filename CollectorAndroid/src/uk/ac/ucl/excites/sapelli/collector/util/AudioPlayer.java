package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.File;
import java.io.IOException;

import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.util.Log;

/**
 * Class that uses the Media Player to play audio files (i.e. mp3)
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class AudioPlayer
{
	
	static private final String TAG = "AudioPlayer";
	
	private Context context;
	private MediaPlayer mediaPlayer;

	public AudioPlayer(Context context)
	{
		this.context = context;
	}

	/**
	 * Use Media Player to play a given audio file
	 * 
	 * @param soundFile
	 * @throws IOException
	 * @throws IllegalStateException
	 */
	public void play(File soundFile)
	{
		// Stop any previous playbacks
		stop();

		try
		{
			if(soundFile != null && soundFile.exists()) // check if the file really exists
			{
				// Get a media player instance:
				mediaPlayer = MediaPlayer.create(context, Uri.fromFile(soundFile));
				
				// Play the sound
				mediaPlayer.start();
				mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
				mediaPlayer.setOnCompletionListener(new OnCompletionListener()
				{
					@Override
					public void onCompletion(MediaPlayer mediaPlayer)
					{
						mediaPlayer.release();
					}
				});
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon playing sound file.", e);
		}
	}
	
	/**
	 * Stop the Media Player
	 */
	public void stop()
	{
		try
		{
			if(mediaPlayer != null && mediaPlayer.isPlaying())
			{
				mediaPlayer.stop();
				mediaPlayer.release();
			}
		}
		catch(Exception ignore) {}
	}

	/**
	 * Destroy the media player and release any resources
	 */
	public void destroy()
	{
		stop();
		mediaPlayer = null;
	}
	
}
