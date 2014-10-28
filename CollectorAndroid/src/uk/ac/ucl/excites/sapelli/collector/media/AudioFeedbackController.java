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
	private Semaphore playbackCompleted; // semaphore used to notify when the media player has finished playing the current track

	public AudioFeedbackController(CollectorController controller) {
		this.controller = controller;

		textToVoice = new TextToVoice(controller.activity.getBaseContext(), controller.activity.getResources().getConfiguration().locale);
		textToVoice.setOnUtteranceProgressListener(this);
	}

	/**
	 * When the media player completes, notify the player thread by releasing the playback semaphore.
	 */
	@Override
	public void onCompletion(MediaPlayer mp) {
		playbackCompleted.release();
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
	 * if necessary (if the file was a temporary file used to store synthesised text).
	 */
	private void playNextQueueItem() {

		// get the next track from the front of the queue
		AudioDescription currentTrack = mediaQueue.get(0);
		try {
			// wait for synthesis to have finished on the media file
			currentTrack.completedSemaphore.acquire();
		} catch (InterruptedException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		// set the media player's source to the new audio track
		Log.d(TAG,"Queuing next audio item: "+currentTrack.text+" ||| "+currentTrack.mediaFile.getName());
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
			// TODO Auto-generated catch block
			Log.e(TAG,"Error when trying to change media player data source to: "+currentTrack.text+", file "+currentTrack.mediaFile.getName(), e);
			playbackCompleted.release();
		}
		
		// animate the view corresponding to the played choice, if necessary:
		if (currentTrack.toAnimate != null) {
			Log.d(TAG,"Animating view for "+currentTrack.text+" || "+currentTrack.mediaFile.getName());
			ViewAnimator.shakeAnimation(controller.activity.getBaseContext(), currentTrack.toAnimate);
		} else {
			Log.d(TAG,"Didn't animate view for "+currentTrack.text+" || "+currentTrack.mediaFile.getName()+" as it was null");
		}

		
		try {
			// wait for the media player to finish playing the track
			playbackCompleted.acquire();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// delete the track's media file if it was a synthesised TTS file:
		if (currentTrack.tts)
			currentTrack.mediaFile.delete();
		
		// remove played item from queue:
		mediaQueue.remove(currentTrack);


	}

	public void playQuestion(Context context, ChoiceField choice, ChoiceView choiceView) {
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();
		if(audioFeedback != null)
		{
			AudioDescription question;
			switch(audioFeedback)
			{
			case LONG_CLICK:
				// TODO play question out loud
				break;
			case SEQUENTIAL:
				mediaQueue = new ArrayList<AudioDescription>();
				question = audioDescFromChoiceField(choice, true, null);
				mediaQueue.add(question);
				List<ChoiceField> children = choice.getChildren();
				for (int i = 0; i < children.size(); i++) {
					mediaQueue.add(audioDescFromChoiceField(children.get(i), false, choiceView.getChildViewAt(i)));
				}

				playbackCompleted = new Semaphore(0);

				Thread playerThread = new Thread() {
					@Override
					public void run() {
						while (!mediaQueue.isEmpty()) {
							playNextQueueItem();
						}
					}
				};
				playerThread.start();
				break;
			case NONE:
				break;
			}
		}
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

				AudioDescription answer = audioDescFromChoiceField(choice, false, choiceView);
				// TODO play answer

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
	 * Destroy the media player and clear the queue of media items.
	 */
	public void stopAudioFeedback() {
		if (queuePlayer != null) {
			queuePlayer.stop();
			queuePlayer.release();
			queuePlayer = null;
		}
		mediaQueue = null;
		playbackCompleted = null;
	}

	/**
	 * Create an AudioDescription object from the provided ChoiceField.
	 * @param choice - the ChoiceField which the audio describes
	 * @param question - whether or not the ChoiceField is the question in this context
	 * @param toAnimate - the view to animate when the audio description is played
	 * @return an AudioDescription object for these parameters
	 */
	private AudioDescription audioDescFromChoiceField(ChoiceField choice, boolean question, View toAnimate) {
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
		private Semaphore completedSemaphore; // semaphore to indicate when this track's file has been fully synthesised
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
			completedSemaphore = new Semaphore(1); // no synthesis necessary, so immediately mark track as "completed"
			this.toAnimate = toAnimate;
		}

		/**
		 * Constructor used when creating object from text to be synthesised.
		 * @param text - the text to synthesise
		 * @param toAnimate - the view to animate when the audio is played
		 */
		AudioDescription(String text, View toAnimate) {
			this.text = text;
			this.tts = true;
			completedSemaphore = new Semaphore(0); // only mark track as "completed" when the TTS engine is finished with it
			this.toAnimate = toAnimate;
			try {
				// create a temporary file in which to store the synthesised audio:
				mediaFile = File.createTempFile("tmp"+Integer.toString(text.hashCode()), null, controller.getFileStorageProvider().getTempFolder(true));
				Log.d(TAG,"Submitting text for processing: "+text);
				// send the text and file off to the TTS engine for synthesis:
				if (textToVoice.processSpeechToFile(text, mediaFile.getAbsolutePath()) == TextToSpeech.ERROR)  {
					Log.e(TAG,"Error when trying to process speech: "+text+". Skipping to next.");
					// if processing fails, allow the media player to skip the file (TODO - improve)
					completedSemaphore.release();
				} else 
					Log.d(TAG,"Speech successfully queued for processing: "+text);
			} catch (Exception e) {
				Log.e(TAG,"Error when trying to process speech: "+text+". Skipping to next.");
				// if processing fails, allow the media player to skip the file (TODO - improve)
				completedSemaphore.release();
			}
		}

		/**
		 *  Mark the audio track as ready to be played by releasing its semaphore (call this when the TTS job is completed).
		 */
		private void complete() {
			if (tts) {
				// reflect that the TTS synthesis job is complete by releasing the semaphore, thus awakening the player thread
				Log.d(TAG,"TTS completed: "+mediaFile.getName()+"  ||  "+text);
				completedSemaphore.release();
			}
		}
	}
}