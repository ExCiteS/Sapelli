package uk.ac.ucl.excites.sapelli.collector.media;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ViewAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.AndroidChoiceUI.PageView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.util.TextToVoice;
import android.content.Context;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;
import android.view.View;

/**
 * Controller for the AudioFeedback Feature
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class AudioFeedbackController extends UtteranceProgressListener
{
	private static CollectorController controller; //TODO why static?
	private static String TAG = "AudioFeedbackController";
	private TextToVoice textToVoice;
	private Locale locale;
	private ArrayList<AudioFeedbackDescription> answersQueue;

	public AudioFeedbackController(CollectorController controller)
	{
		AudioFeedbackController.controller = controller;
		locale = controller.activity.getResources().getConfiguration().locale;
		
		textToVoice = new TextToVoice(controller.activity.getBaseContext(), locale);
	}
	
	/**
	 * Speaks aloud the description of a ChoiceField used to ask the question. The description used can be either audio file or text that uses Android's TTS
	 * (Text-To-Speech) engine to read it aloud which is defined in the XML of a project.
	 * 
	 * @param choice
	 */
	public void playQuestion(Context context, ChoiceField choice, PageView pageView)
	{
		Log.d(TAG,"Play question");
		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();

		if(audioFeedback != null)
		{
			AudioFeedbackDescription question;
			switch(audioFeedback)
			{
			case LONG_CLICK:
				// play question:
				question = AudioFeedbackDescription.afdFromChoiceField(choice, textToVoice, true);
				question.play();
				break;

			case SEQUENTIAL:
				answersQueue = new ArrayList<AudioFeedbackDescription>();
				// add question to queue:
				question = AudioFeedbackDescription.afdFromChoiceField(choice, textToVoice, true);
				synchronized(answersQueue) {
					answersQueue.add(question);
				}
				// then add all possible answers:
				List<ChoiceField> children  = choice.getChildren();
				synchronized(answersQueue) {
					for (int i = 0; i < children.size(); i++) {
						answersQueue.add(AudioFeedbackDescription.afdFromChoiceField(children.get(i), textToVoice, false));
					}
				}
				// play all items in the queue. Do this on a new thread because it will be blocked when
				// it encounters an item whose TTS has not yet been synthesised
				Thread playerThread = new Thread() {
					@Override
					public void run() {
						synchronized(answersQueue) {
							while (!answersQueue.isEmpty()) {
								AudioFeedbackDescription nextAnswer = answersQueue.remove(0); //TODO concurrency
								nextAnswer.play();
								if (nextAnswer.tts) {
									// if item was TTS, delete its temporary file now it has been played:
									nextAnswer.file.delete();
								}
							}
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

				AudioFeedbackDescription answer = AudioFeedbackDescription.afdFromChoiceField(choice, textToVoice, false);
				answer.play();

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
	 * Use the Android TTS (Text-To-Speech) Engine to speak the text
	 * 
	 * @param text
	 */
	public void textToVoice(String text)
	{
		Log.d(TAG,"Text to voice: "+text);
		if(textToVoice == null) {
			Log.d(TAG,"Text to voice was null");
			return;
		}
		File file;
        try {
	        file = File.createTempFile(Integer.toString(text.hashCode()), null, controller.getFileStorageProvider().getTempFolder(true));
			// QUEUE the text for synthesis (which is conducted asynchronously)
			if (textToVoice.processSpeechToFile(text, file.getAbsolutePath()) != TextToSpeech.SUCCESS) {
				Log.e(TAG,"Error when trying to save synthesised speech to disk.");
			}
        } catch (FileStorageException e) {
	        // TODO Auto-generated catch block
	        e.printStackTrace();
        } catch (IOException e) {
	        // TODO Auto-generated catch block
	        e.printStackTrace();
        }
	}

	public void stopAudioFeedback()
	{
		// Stop the Media Player
		controller.stopAudio();

		// Stop the Android TTS (Text-To-Speech) Engine
		if(textToVoice != null)
			textToVoice.stop();
	}

	public void destroy() {
		if (textToVoice != null) {
			textToVoice.destroy();
		}
		textToVoice = null;
	}
	
	@Override
    public void onDone(String utteranceId) {
		// mark the appropriate 
	    for (AudioFeedbackDescription answer : answersQueue) {
	    	if (answer.text == utteranceId)
	    		answer.complete();
	    }
    }
	
	@Override
    public void onStart(String utteranceId) {
	    // nothing to do
    }

	@Override
    public void onError(String utteranceId) {
		// nothing to do
    }
	
	private static class AudioFeedbackDescription {
		
		private String text = "";
		private Semaphore completedSemaphore = new Semaphore(0);
		private boolean tts;
		private File file;
		
		AudioFeedbackDescription(File soundFile) {
			this.file = soundFile;
			tts = false;
			// can play straight away, reflect this by releasing a permit to the semaphore:
			completedSemaphore.release();
		}
		
		AudioFeedbackDescription(String text, TextToVoice ttv) {
			this.text = text;
			tts = true;
			File file;
	        try {
		        file = File.createTempFile(Integer.toString(text.hashCode()), null, controller.getFileStorageProvider().getTempFolder(true));
				if (ttv.processSpeechToFile(text, file.getAbsolutePath()) == TextToSpeech.ERROR)  {
					completedSemaphore.release(); // processing has failed so skip this file TODO
				}
	        } catch (FileStorageException e) {
		        // TODO Auto-generated catch block
		        e.printStackTrace();
	        } catch (IOException e) {
		        // TODO Auto-generated catch block
		        e.printStackTrace();
	        }
		}
		
		private static AudioFeedbackDescription afdFromChoiceField(ChoiceField choice, TextToVoice ttv, boolean question) {
			if (question) {
				if(choice.hasAudioQuestionDesc())
					return new AudioFeedbackDescription(controller.getProject().getSoundFile(controller.getFileStorageProvider(),choice.getAnswerDesc()));
				else if(choice.getQuestionDesc() != null)
					// Enable TTS Audio Feedback
					return new AudioFeedbackDescription(choice.getQuestionDesc(), ttv);
				else
					// Enable TTS Audio Feedback
					return new AudioFeedbackDescription(choice.getAltText(), ttv);
			}
			else {
					if(choice.hasAudioAnswerDesc())
					return new AudioFeedbackDescription(controller.getProject().getSoundFile(controller.getFileStorageProvider(),choice.getAnswerDesc()));
				else if(choice.getAnswerDesc() != null)
					// Enable TTS Audio Feedback
					return new AudioFeedbackDescription(choice.getAnswerDesc(), ttv);
				else
					// Enable TTS Audio Feedback
					return new AudioFeedbackDescription(choice.getAltText(), ttv);
			}
		}
		
		private void complete() {
			if (tts) {
				// reflect that the TTS synthesis job is complete by releasing the semaphore, thus awakening the player thread
				Log.d(TAG,"TTS completed: "+file.getName()+"  ||  "+text);
				completedSemaphore.release();
			}
		}
		
		private void play() {
			try {
				Log.d(TAG,"Trying to acquire semaphore for "+file.getName()+"  ||  "+text);
	            completedSemaphore.acquire();
            } catch (InterruptedException e) {
	            e.printStackTrace();
            }
			Log.d(TAG,"Semaphore acquired for "+file.getName()+"  ||  "+text+". Playing sound");
			controller.playSound(file, true);   
		}
		
	}
}
