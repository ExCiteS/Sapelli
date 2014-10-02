package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.IOException;

import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;

/**
 * Class that uses the Media Player to speak a given audio file (i.e. mp3)
 * 
 * @author Michalis Vitos
 *
 */
public class AudioToVoice
{
	private Context context;
	private MediaPlayer mediaPlayer;

	public AudioToVoice(Context context)
	{
		this.context = context;
	}

	/**
	 * Use Media Player to speak a given audio file
	 * 
	 * @param soundFilePath
	 * @throws IOException
	 * @throws IllegalStateException
	 */
	public void speak(String soundFilePath)
	{
		// Stop any previous playbacks
		stop();

		// Get the volume
		final AudioManager audioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
		final int originalVolume = audioManager.getStreamVolume(AudioManager.STREAM_MUSIC);
		final int streamMaxVolume = audioManager.getStreamMaxVolume(AudioManager.STREAM_MUSIC);

		// Set the volume
		audioManager.setStreamVolume(AudioManager.STREAM_MUSIC, streamMaxVolume, 0);

		// Create the Media Player
		if(soundFilePath != null)
		{
			mediaPlayer = MediaPlayer.create(context, Uri.parse(soundFilePath));
			mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
		}

		// Make sure that the Media Player was successfully created.
		if(mediaPlayer != null)
		{
			mediaPlayer.start();

			// Set the volume back to its original value
			mediaPlayer.setOnCompletionListener(new OnCompletionListener()
			{
				@Override
				public void onCompletion(MediaPlayer mp)
				{
					audioManager.setStreamVolume(AudioManager.STREAM_MUSIC, originalVolume, 0);
				}
			});
		}
	}

	/**
	 * Stop the Media Player
	 */
	public void stop()
	{
		if(mediaPlayer != null && mediaPlayer.isPlaying())
			mediaPlayer.stop();
	}

	/**
	 * Destroy the media player and release any resources
	 */
	public void destroy()
	{
		if(mediaPlayer != null)
		{
			mediaPlayer.release();
			mediaPlayer = null;
		}
	}
}
