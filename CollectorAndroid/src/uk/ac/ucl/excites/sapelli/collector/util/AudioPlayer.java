package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.util.Log;

/**
 * Class that uses the Media Player to play audio files (i.e. mp3)
 * 
 * @author Michalis Vitos, mstevens, benelliott
 *
 */
public class AudioPlayer
{

	static private final String TAG = "AudioPlayer";

	private Context context;
	private MediaPlayer immediatePlayer;
	private ArrayList<File> mediaQueue;
	private MediaPlayer queuePlayer;
	private boolean queuePlaying = false;

	public AudioPlayer(Context context)
	{
		this.context = context;
	}

	/**
	 * Use Media Player to play a given audio file immediately (stopping whatever was already
	 * playing immediately, but playing alongside any queued media).
	 * 
	 * @param soundFile
	 * @throws IOException
	 * @throws IllegalStateException
	 */
	public void playImmediate(File soundFile)
	{
		// Stop any previous playbacks
		stopImmediate();

		try
		{
			if(soundFile != null && soundFile.exists()) // check if the file really exists
			{
				// Get a media player instance:
				immediatePlayer = MediaPlayer.create(context, Uri.fromFile(soundFile));

				// Play the sound
				immediatePlayer.start();
				immediatePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
				immediatePlayer.setOnCompletionListener(new OnCompletionListener()
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
	 * Stop the immediate media player
	 */
	public void stopImmediate()
	{
		try
		{
			if(immediatePlayer != null && immediatePlayer.isPlaying())
			{
				immediatePlayer.stop();
				immediatePlayer.release();
			}
		}
		catch(Exception ignore) {}
	}

	/**
	 * Destroy the media player and release any resources
	 */
	public void destroy()
	{
		stopImmediate();
		stopQueue();
		immediatePlayer = null; //TODO move this?
	}

	/**
	 * Play all items that have been queued.
	 */
	public void playQueue() {
		if (setNextQueueItem()) {
			// if queue is not empty...
			
			// set player to play next queue item on completion:
			queuePlayer.setOnCompletionListener(new OnCompletionListener() {
				@Override
				public void onCompletion(MediaPlayer mp) {
					Log.d("TAG","On completion. Queue length: "+mediaQueue.size());
					// set player's data source:
					if (setNextQueueItem())
						// if queue not empty, play next item:
						queuePlayer.start();
					else
						queuePlaying = false;
				}
			});
			
			// start playing from the queue:
			queuePlaying = true;
			queuePlayer.start();
		} else {
			queuePlaying = false;
		}
	}
	
	/**
	 * Sets the queue media player's data source to the next item in the queue, if it exists.
	 * @return true if the data source was successfully set as the next item in the queue, false otherwise.
	 */
	private boolean setNextQueueItem() {
		if (!mediaQueue.isEmpty()) {
			File nextSound = mediaQueue.remove(0);
			Log.d(TAG,"Setting queue player's data source to: "+nextSound.getAbsolutePath());
			try {
				if (queuePlayer == null) {
					queuePlayer = MediaPlayer.create(context, Uri.fromFile(nextSound));
					queuePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
				}
				else {
					queuePlayer.reset(); // reset to idle state
					queuePlayer.setDataSource(context, Uri.fromFile(nextSound));
					queuePlayer.prepare();
				}
            } catch (Exception e) {
            	Log.e(TAG,"Error when trying to play media file: "+nextSound.getAbsolutePath(), e);
            	return false;
            }
			return true;
		}
		return false;
	}

	/**
	 * Stop playing queued items.
	 */
	public void stopQueue() {
		if (queuePlayer != null) {
			queuePlaying = false;
			queuePlayer.stop();
			queuePlayer.release();
			queuePlayer = null;
		}
	}

	/**
	 * Add a new media file to the queue of files to be played.
	 * @param mediaFile
	 */
	public void enqueueAndPlay(File mediaFile) {
		Log.d(TAG,"Enqueuing: "+mediaFile.getAbsolutePath());
		if (mediaQueue == null) {
			mediaQueue = new ArrayList<File>();
		}
		mediaQueue.add(mediaFile);
		if (!queuePlaying) {
			playQueue();
		}
	}
}
