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
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class AudioFeedbackController extends UtteranceProgressListener implements OnCompletionListener  {

	private static String TAG = "AudioFeedbackController";
	private CollectorController controller;
	private MediaPlayer queuePlayer;
	private ArrayList<AudioDescription> mediaQueue;
	private TextToVoice textToVoice;
	private Semaphore playbackCompleted;

	public AudioFeedbackController(CollectorController controller) {
		this.controller = controller;

		textToVoice = new TextToVoice(controller.activity.getBaseContext(), controller.activity.getResources().getConfiguration().locale);
		textToVoice.setOnUtteranceProgressListener(this);

		playbackCompleted = new Semaphore(0);
	}

	@Override
	public void onCompletion(MediaPlayer mp) {
		playbackCompleted.release();
	}


	@Override
	public void onStart(String utteranceId) {
		Log.d(TAG,"Started processing TTS: "+utteranceId);
	}

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

	@Override
	public void onError(String utteranceId) {
		Log.d(TAG,"Error processing TTS: "+utteranceId);	    
	}

	private void playNextQueueItem() {
		
		AudioDescription next = mediaQueue.get(0);
		try {
	        next.completedSemaphore.acquire();
        } catch (InterruptedException e1) {
	        // TODO Auto-generated catch block
	        e1.printStackTrace();
        }
		
		if (next != null) {
			Log.d(TAG,"Queuing next audio item: "+next.text+" ||| "+next.mediaFile.getName());
			try {
				// set the media player's data source:
				if (queuePlayer == null) {
					queuePlayer = MediaPlayer.create(controller.activity.getBaseContext(), Uri.fromFile(next.mediaFile));
					queuePlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
					queuePlayer.setOnCompletionListener(this);
				}
				else {
					queuePlayer.reset(); // reset to idle state
					queuePlayer.setDataSource(controller.activity.getBaseContext(), Uri.fromFile(next.mediaFile));
					queuePlayer.prepare();

				}
				// start it playing:
				queuePlayer.start();
				
			} catch (Exception e) {
				// TODO Auto-generated catch block
				Log.e(TAG,"Error when trying to change media player data source to: "+next.text+", file "+next.mediaFile.getName(), e);
				playbackCompleted.release();
			}
			// animate its view if necessary:
			if (next.toAnimate != null) {
				Log.d(TAG,"Animating view for "+next.text+" || "+next.mediaFile.getName());
				ViewAnimator.shakeAnimation(controller.activity.getBaseContext(), next.toAnimate);
			} else {
				Log.d(TAG,"Didn't animate view for "+next.text+" || "+next.mediaFile.getName()+" as it was null");
			}
		}
		
		Log.d(TAG,"About to try to acquire playback semaphore...");
		try {
			// wait for the previous track to have been completed:
			playbackCompleted.acquire();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Log.d(TAG,"Playback semaphore acquired");
		// delete previous file if it was a synthesised TTS file:
		if (next.tts){}
			//next.mediaFile.delete(); TODO
		// remove played item from queue:
		mediaQueue.remove(next);

		
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
				Log.d(TAG,"Items added to queue");
				Thread playerThread = new Thread() {
					@Override
					public void run() {
						while (!mediaQueue.isEmpty()) {
							playNextQueueItem();
							// play next queue item until there is none left
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


	public void stopAudioFeedback() {
		// stop media player
		// clear queue
	}
	
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

	private class AudioDescription {
		private File mediaFile;
		private String text;
		private boolean tts;
		private Semaphore completedSemaphore;
		private View toAnimate;

		AudioDescription(File mediaFile, View toAnimate) {
			this.mediaFile = mediaFile;
			this.text = "";
			this.tts = false;
			completedSemaphore = new Semaphore(1);
			this.toAnimate = toAnimate;
		}

		AudioDescription(String text, View toAnimate) {
			this.text = text;
			this.tts = true;
			completedSemaphore = new Semaphore(0);
			this.toAnimate = toAnimate;
			try {
				mediaFile = File.createTempFile("tmp"+Integer.toString(text.hashCode()), null, controller.getFileStorageProvider().getTempFolder(true));
				Log.d(TAG,"Submitting text for processing: "+text);
				if (textToVoice.processSpeechToFile(text, mediaFile.getAbsolutePath()) == TextToSpeech.ERROR)  {
					Log.e(TAG,"Error when trying to process speech: "+text+". Skipping to next.");
					completedSemaphore.release(); // processing has failed so skip this file TODO
				} else 
					Log.d(TAG,"Speech successfully queued for processing: "+text);
			} catch (Exception e) {
				Log.e(TAG,"Error when trying to process speech: "+text+". Skipping to next.");
				completedSemaphore.release(); // processing has failed so skip this file TODO
			}
		}
		




		private void complete() {
			if (tts) {
				// reflect that the TTS synthesis job is complete by releasing the semaphore, thus awakening the player thread
				Log.d(TAG,"TTS completed: "+mediaFile.getName()+"  ||  "+text);
				completedSemaphore.release();
			}
		}
	}

	public void destroy() {
	    // TODO Auto-generated method stub
	    
    }
}