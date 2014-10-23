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
	private ArrayList<SoundFile> mediaQueue;
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
		immediatePlayer = null;
		queuePlayer = null;
	}

	/**
	 * Play all items that have been queued.
	 */
	public void playQueue() {
		if (initialiseQueuePlayer()) {
			Log.d(TAG,"Playing...");
			queuePlayer.setOnCompletionListener(new OnCompletionListener() {
				@Override
				public void onCompletion(MediaPlayer mp) {
					Log.d("TAG","On completion. Queue length: "+mediaQueue.size());
					if (mediaQueue.get(0).deleteAfterPlaying)
						mediaQueue.get(0).file.delete();
					mediaQueue.remove(0);
					initialiseQueuePlayer();
					queuePlayer.start();
				}
			});
			queuePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
			queuePlayer.start();
		}
	}
	
	public void playQueue2() {
		while (queuePlaying) {
			// TODO
		}
			
	}

	/**
	 * Stop playing queued items.
	 */
	public void stopQueue() {
		if (queuePlayer != null) {
			queuePlaying = false;
			queuePlayer.stop();
			queuePlayer.release();
		}
	}

	private boolean initialiseQueuePlayer() {
		if (!mediaQueue.isEmpty()) {
			Log.d(TAG,"Initialising queue player from file: "+mediaQueue.get(0).file.getAbsolutePath());
			try {
				if (queuePlayer == null)
					queuePlayer = MediaPlayer.create(context, Uri.fromFile(mediaQueue.get(0).file));
				else
					queuePlayer.setDataSource(context, Uri.fromFile(mediaQueue.get(0).file));
            } catch (IllegalArgumentException e) {
	            // TODO Auto-generated catch block
	            e.printStackTrace();
            } catch (SecurityException e) {
	            // TODO Auto-generated catch block
	            e.printStackTrace();
            } catch (IllegalStateException e) {
	            // TODO Auto-generated catch block
	            e.printStackTrace();
            } catch (IOException e) {
	            // TODO Auto-generated catch block
	            e.printStackTrace();
            }
			queuePlayer = MediaPlayer.create(context, Uri.fromFile(mediaQueue.get(0).file));
			return true;
		}
		return false;
	}

	/**
	 * Add a new media file to the queue of files to be played.
	 * @param mediaFile
	 */
	public void enqueueAndPlay(File mediaFile, boolean deleteAfterPlaying) {
		Log.d(TAG,"Enqueuing: "+mediaFile.getAbsolutePath());
		if (mediaQueue == null) {
			mediaQueue = new ArrayList<SoundFile>();
		}
		mediaQueue.add(new SoundFile(mediaFile, deleteAfterPlaying));
		if (!queuePlaying) {
			playQueue();
			queuePlaying = true;
		}
	}

	private class SoundFile {
		File file;
		boolean deleteAfterPlaying = false;

		SoundFile(File file, boolean deleteAfterPlaying) {
			this.file = file;
			this.deleteAfterPlaying = deleteAfterPlaying;
		}
	}

}
