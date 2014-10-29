package uk.ac.ucl.excites.sapelli.collector.media;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ViewAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.AndroidChoiceUI.ChoiceView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.util.TTSInitListener;
import uk.ac.ucl.excites.sapelli.collector.util.TextToVoice;
import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;
import android.view.View;

/**
 * Class which plays the audio descriptions of choice fields.
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class AudioFeedbackController extends UtteranceProgressListener implements OnCompletionListener  {

	private static String TAG = "AudioFeedbackController";
	private CollectorController controller;
	private MediaPlayer queuePlayer;
	private ArrayList<AudioDescription> mediaQueue; // list of audio to play
	private TextToVoice textToVoice;
	private Thread playbackThread;
	private Semaphore audioAvailableSem; // semaphore used to notify when there is a track in the queue to be played
	private Semaphore playbackCompletedSem; // semaphore used to notify when the media player has finished playing the current track
	private volatile boolean running = false; // boolean used to start/stop the playback thread looping

	public AudioFeedbackController(CollectorController controller) {
		this.controller = controller;

		textToVoice = new TextToVoice(controller.activity.getBaseContext(), controller.activity.getResources().getConfiguration().locale);
		textToVoice.setOnUtteranceProgressListener(this);		
	}

	/**
	 * Start a thread which constantly pulls audio tracks from the queue and plays them in-order.
	 */
	public void startPlayingFeedback() {
		Log.d(TAG, "Starting playback...");
		playbackCompletedSem = new Semaphore(0);

		if (playbackThread == null) { // which it always will be since the thread is nullified when stopped
			playbackThread = new Thread() {
				@Override
				public void run() {
					while (running) {
						playNextQueueItem();
					}
				}
			};
		}

		running = true;
		playbackThread.start();
	}

	/**
	 * Destroy the media player and clear the queue of media items.
	 */
	public void stopPlayingFeedback() {
		Log.d(TAG,"Stopping playback...");
		running = false;
		// destroy audio player:
		if (queuePlayer != null) {
			queuePlayer.stop();
			queuePlayer.release();
			queuePlayer = null;
		}
		// interrupt playback thread since it is probably blocked on a semaphore:
		playbackThread.interrupt();
		try {
			// wait for playback thread to die before continuing:
			playbackThread.join();
		} catch (InterruptedException e) {
			Log.e(TAG,"Main thread interrupted while waiting for playback thread to join.");
		}
		// nullify thread so it must be re-initialised:
		playbackThread = null;
		// destroy queue so stale media cannot be played later:
		mediaQueue = null;
		// destroy semaphores so they aren't reused by later ChoiceFields:
		audioAvailableSem = null;
		playbackCompletedSem = null;
	}

	/**
	 * When the media player completes, notify the player thread by releasing the playback semaphore.
	 */
	@Override
	public void onCompletion(MediaPlayer mp) {
		playbackCompletedSem.release();
	}


	/**
	 * Called when the text-to-speech engine starts processing a particular utterance.
	 */
	@Override
	public void onStart(String utteranceId) {
		Log.d(TAG,"Started processing TTS: "+utteranceId);
	}

	/**
	 * Called when the text-to-speech engine finishes processing a specific utterance. Mark the
	 * corresponding item in the queue as being completed, so that the player thread is unblocked and can play it.
	 */
	@Override
	public void onDone(String utteranceId) {
		Log.d(TAG, "Finished processing TTS: "+utteranceId);
		// find TTS job in queue:
		for (AudioDescription ad : mediaQueue) {
			if (ad.text.equals(utteranceId)) {
				// mark the matching job as done so it can be played:
				ad.complete();
				// stop searching the queue:
				break;
			}
		}

	}

	/**
	 * Called when the text-to-speech engine encounters an error while processing a speicfic utterance.
	 */
	@Override
	public void onError(String utteranceId) {
		Log.d(TAG,"Error processing TTS: "+utteranceId);	    
	}

	/**
	 * Play the next item in the queue, and once it has finished, remove it from the queue and delete the file
	 * if necessary (if the file was a temporary file used to store synthesised text). Precisely, this method executes the
	 * following steps:
	 * <ul>
	 * <li> Wait for a new track (acquire {@link AudioFeedbackController#audioAvailableSem}) </li>
	 * <li> Wait for track to be ready - i.e. for TTS synthesis to finish (acquire {@link AudioFeedbackController.AudioDescription#audioProcessedSem}) </li>
	 * <li> Start playing track </li>
	 * <li> Wait for playback to finish (acquire {@link AudioFeedbackController#playbackCompletedSem}) </li>
	 * <li> Remove the just-played item from the queue and delete its file if it was a TTS synthesis </li>
	 * </ul>
	 */
	private void playNextQueueItem() {
		try {
			// wait for a track to become available:
			audioAvailableSem.acquire();
			// get the next track from the front of the queue but do not remove it (as otherwise the synthesis job
			// may not be able to find the track in the queue)
			final AudioDescription currentTrack;
			synchronized(mediaQueue) {
				currentTrack = mediaQueue.get(0); 
			}
			// wait for synthesis to have finished on the media file:
			currentTrack.audioProcessedSem.acquire();
			// set the media player's source to the new audio track
			Log.d(TAG,"Queuing next audio item: "+currentTrack.text+" || "+currentTrack.mediaFile.getName());
			try {
				if (queuePlayer == null) {
					queuePlayer = MediaPlayer.create(controller.activity.getBaseContext(), Uri.fromFile(currentTrack.mediaFile));
					queuePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
					queuePlayer.setOnCompletionListener(this);
				}
				else {
					queuePlayer.reset(); // reset to idle state
					queuePlayer.setDataSource(controller.activity.getBaseContext(), Uri.fromFile(currentTrack.mediaFile));
					queuePlayer.prepare();

				}
				// start it playing:
				queuePlayer.start();

			} catch (Exception e) {
				Log.e(TAG,"Error when trying to change media player data source to: "+currentTrack.text+", file "+currentTrack.mediaFile.getName(), e);
				// Playing failed so completed listener will never fire. Allow file to be deleted and playback to continue:
				playbackCompletedSem.release();
			}
			// animate the view corresponding to the played choice, if necessary:
			if (currentTrack.toAnimate != null) {
				Log.d(TAG,"Animating view for "+currentTrack.text+" || "+currentTrack.mediaFile.getName());
				controller.activity.runOnUiThread(new Runnable() { // TODO improve

					@Override
                    public void run() {
						ViewAnimator.shakeAnimation(controller.activity.getBaseContext(), currentTrack.toAnimate);
                    }
					
				});
				
			} else {
				Log.d(TAG,"Didn't animate view for "+currentTrack.text+" || "+currentTrack.mediaFile.getName()+" as it was null");
			}

			// wait for the media player to finish playing the track
			playbackCompletedSem.acquire();

			// delete the track's media file if it was a synthesised TTS file:
			if (currentTrack.tts)
				currentTrack.mediaFile.delete();

			// remove played item from queue:
			synchronized(mediaQueue) {
				mediaQueue.remove(currentTrack);
			}
		} catch (InterruptedException e1) {
			// was probably caused by the ChoiceField being exited
			Log.d(TAG,"Playback thread interrupted while waiting for semaphore.");
		}

	}

	public void playQuestion(Context context, ChoiceField choice, ChoiceView choiceView) {
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();
		if(audioFeedback != null)
		{
			switch(audioFeedback)
			{
			case LONG_CLICK:

				// enqueue question:
				enqueueDescription(choice, true, null);
				// play media queue:
				startPlayingFeedback();
				break;

			case SEQUENTIAL:

				// enqueue question:
				enqueueDescription(choice, true, null);
				List<ChoiceField> children = choice.getChildren();
				for (int i = 0; i < children.size(); i++) {
					// enqueue each answer:
					enqueueDescription(children.get(i), false, choiceView.getChildViewAt(i));
				}
				// play media queue:
				startPlayingFeedback();
				break;

			case NONE:

				break;

			}
		}
	}
	
	/**
	 * Creates an AudioDescription from the provided parameters then enqueues it if creation was successful.
	 * @param choice - the ChoiceField which the audio describes
	 * @param question - whether or not the ChoiceField is the question in this context
	 * @param toAnimate - the view to animate when the audio description is played
	 */
	private void enqueueDescription(ChoiceField choice, boolean question, View toAnimate) {
		try {
	        AudioDescription ad = audioDescFromChoiceField(choice, question, toAnimate);
	        enqueueDescription(ad);
        } catch (TTSFailedException e) {
	        Log.e(TAG,"Failed to synthesise TTS for text: "+e.getText());
        }
	}

	/**
	 * Adds the provided AudioDescription object to the playback queue, 
	 * creating the queue first if it does not already exist and notifying the availability
	 * of a new track by releasing the {@code tracksInQueue} semaphore.
	 * @param audioDesc - the AudioDescription to queue
	 */
	private void enqueueDescription(AudioDescription audioDesc) {
		if (mediaQueue == null)
			mediaQueue = new ArrayList<AudioDescription>();
		if (audioAvailableSem == null)
			audioAvailableSem = new Semaphore(0);
		synchronized(mediaQueue) {
			mediaQueue.add(audioDesc);
		}
		audioAvailableSem.release();
	}

	/**
	 * Speaks aloud the description of a ChoiceField and animates the ChoiceField. The description used can be either audio file or text that uses Android's TTS
	 * (Text-To-Speech) engine to read it aloud which is defined in the XML of a project.
	 * 
	 * @param choice
	 * @param choiceView
	 */
	public void playAnswer(Context context, ChoiceField choice, View choiceView)
	{
		Log.d(TAG,"Play answer");
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();

		if(audioFeedback != null)
		{
			switch(audioFeedback)
			{
			case LONG_CLICK:
			case SEQUENTIAL:

				// enqueue answer:
				enqueueDescription(choice, false, choiceView);
				// start playback:
				//startPlayingFeedback();
				break;

			case NONE:
				controller.addLogLine("LONG_CLICK", "LongClick on " + choice.getAltText() + " but AudioFeedback is disabled");
				return;
			}

			// Apply an alpha animation to the long pressed view
			//ViewAnimator.shakeAnimation(context, choiceView); TODO
		}
	}

	/**
	 * Speaks aloud the description of a ControlItem and animates the ControlItem. The description used can be either audio file or text that uses Android's TTS
	 * (Text-To-Speech) engine to read it aloud which is defined in the XML of a project.
	 * 
	 * @param controlItem
	 * @param controlView
	 */
	public void playAnswer(Context context, Item controlItem, View controlView)
	{
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();

		if(audioFeedback != null)
		{
			switch(audioFeedback)
			{
			case LONG_CLICK:
			case SEQUENTIAL:

				// TODO: decide TTS / MediaPlayer


				// If the choice has an audio, pass that audio to the Media Player
				if(false)
					return;
				else
					// Enable TTS Audio Feedback
					//textToVoice(controlItem.getDescription()); TODO
					break;

			case NONE:
				controller.addLogLine("LONG_CLICK", "LongClick on " + controlItem.getDescription() + " but AudioFeedback is disabled");
				return;
			}

			// Apply an alpha animation to the long pressed view
			ViewAnimator.shakeAnimation(context, controlView);
		}
	}

	/**
	 * Create an AudioDescription object from the provided ChoiceField.
	 * @param choice - the ChoiceField which the audio describes
	 * @param question - whether or not the ChoiceField is the question in this context
	 * @param toAnimate - the view to animate when the audio description is played
	 * @return an AudioDescription object for these parameters
	 */
	private AudioDescription audioDescFromChoiceField(ChoiceField choice, boolean question, View toAnimate) throws TTSFailedException {
		if (question) {
			if(choice.hasAudioQuestionDesc())
				return new AudioDescription(controller.getProject().getSoundFile(controller.getFileStorageProvider(), choice.getAnswerDesc()), toAnimate);
			else if(choice.getQuestionDesc() != null)
				// Enable TTS Audio Feedback
				return new AudioDescription(choice.getQuestionDesc(), toAnimate);
			else
				// Enable TTS Audio Feedback
				return new AudioDescription(choice.getAltText(), toAnimate);
		}
		else {
			if(choice.hasAudioAnswerDesc())
				return new AudioDescription(controller.getProject().getSoundFile(controller.getFileStorageProvider(), choice.getAnswerDesc()), toAnimate);
			else if(choice.getAnswerDesc() != null)
				// Enable TTS Audio Feedback
				return new AudioDescription(choice.getAnswerDesc(), toAnimate);
			else
				// Enable TTS Audio Feedback
				return new AudioDescription(choice.getAltText(), toAnimate);
		}
	}

	/**
	 * A class which represents a single "audio description" track, which can be created using an existing media file (e.g. mp3)
	 * or by synthesising text.
	 * 
	 * @author Ben
	 *
	 */
	private class AudioDescription {
		private File mediaFile;
		private String text;
		private boolean tts;
		private Semaphore audioProcessedSem; // semaphore to indicate when this track's file has been fully synthesised
		private View toAnimate;

		/**
		 * Constructor used when creating object from a media file (e.g. mp3).
		 * @param mediaFile - the media file 
		 * @param toAnimate - the view to animate when the audio is played
		 */
		AudioDescription(File mediaFile, View toAnimate) {
			this.mediaFile = mediaFile;
			this.text = ""; // no text 
			this.tts = false;
			audioProcessedSem = new Semaphore(1); // no synthesis necessary, so immediately mark track as "completed"
			this.toAnimate = toAnimate;
		}

		/**
		 * Constructor used when creating object from text to be synthesised.
		 * @param text - the text to synthesise
		 * @param toAnimate - the view to animate when the audio is played
		 */
		AudioDescription(String text, View toAnimate) throws TTSFailedException {
			this.text = text;
			this.tts = true;
			audioProcessedSem = new Semaphore(0); // only mark track as "completed" when the TTS engine is finished with it
			this.toAnimate = toAnimate;
			try {
				// create a temporary file in which to store the synthesised audio:
				mediaFile = File.createTempFile("tmp"+Integer.toString(text.hashCode()), null, controller.getFileStorageProvider().getTempFolder(true));
				Log.d(TAG,"Submitting text for processing: "+text);
				// send the text and file off to the TTS engine for synthesis:
				if (textToVoice.processSpeechToFile(text, mediaFile.getAbsolutePath()) == TextToSpeech.ERROR)  {
					throw new Exception();
				} else 
					Log.d(TAG,"Speech successfully queued for processing: "+text);
			} catch (Exception e) {
				throw new TTSFailedException(text);
			}
		}

		/**
		 *  Mark the audio track as ready to be played by releasing its semaphore (call this when the TTS job is completed).
		 */
		private void complete() {
			if (tts) {
				// reflect that the TTS synthesis job is complete by releasing the semaphore, thus awakening the player thread
				Log.d(TAG,"TTS completed: "+mediaFile.getName()+"  ||  "+text);
				audioProcessedSem.release();
			}
		}
	}

	public void setTTSInitListener(TTSInitListener listener) {
	    textToVoice.setTTSInitListener(listener);
    }
}