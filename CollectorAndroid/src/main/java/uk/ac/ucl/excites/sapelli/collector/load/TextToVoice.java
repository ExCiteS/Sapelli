package uk.ac.ucl.excites.sapelli.collector.load;

import android.annotation.TargetApi;
import android.content.Context;
import android.os.Build;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;

import java.io.File;
import java.util.HashMap;
import java.util.Locale;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import timber.log.Timber;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to synthesise a string to a (WAV) file. This class is a synchronous abstraction of Android's
 * asynchronous TTS API - when an object of this type is instantiated or a call is made to synthesise a file the calling thread will block (!) until the job is
 * finished. This class should therefore not be instantiated or used on the main thread or the UI will become unresponsive.
 * 
 * 
 * @author Michalis Vitos, benelliott, mstevens
 */
public class TextToVoice implements TextToSpeech.OnInitListener
{
	
	private Context context;
	private Locale locale;
	private String previousLanguageCode = "";
	private TextToSpeech tts;
	private Semaphore ttvInitialised;
	private Semaphore ttvJobComplete;

	public TextToVoice(Context context)
	{
		this.context = context;
		// TTS engine will be set-up when the first process request arrives
	}

	/**
	 * Synchronous method that blocks the calling thread until the text-to-speech engine has been fully initialised (!).
	 * Thus assumes that the calling thread is *not* the UI thread.
	 */
	private void setupTTS()
	{
		Timber.d("Setting up TTS...");

		// Set up initialisation semaphore so thread blocks waiting for TTS init (else jobs will fail)
		if(ttvInitialised == null)
			ttvInitialised = new Semaphore(0);

		// Instantiate TTS engine and subscribe to updates as to when it is ready (it will then call onInit()):
		tts = new TextToSpeech(context, this);

		// Set up listener for when synthesis jobs complete (pre-API 15 listener is now deprecated):
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1 /* 15 */)
		{
			Timber.d(">= 15 build version detected");
			new UtteranceListener15Plus(tts);
		}
		else
		{
			Timber.d("< 15 build version detected");
			new UtteranceListenerSub15(tts);
		}

		// Block calling thread until the TTS engine is initialised:
		Timber.d("Waiting for init...");
		try
		{
			ttvInitialised.tryAcquire(10, TimeUnit.SECONDS);
		}
		catch(InterruptedException e)
		{
			Timber.e(e);
		}
		Timber.d("Init complete");
	}

	/**
	 * Called when the text-to-speech engine is initialised. Releases the thread that started the initialisation if the engine reports success.
	 */
	@Override
	public void onInit(int status)
	{
		Timber.d("The TTS has been initialized with status code: %s ", status);
		if(status == TextToSpeech.SUCCESS && tts != null)
			ttvInitialised.release();
		// if null, has probably been destroyed before initialisation completed
	}

	/**
	 * Synchronous method that dispatches a text synthesis job to the TTS engine and then blocks the calling thread until synthesis is finished (!). Assumes
	 * that the calling thread is thus *not* the UI thread.
	 * 
	 * @param text - the text to synthesise into speech
	 * @param filepath - the path of the file into which the speech audio should be stored
	 */
	public void processSpeechToFile(String text, String filepath, String languageCode) throws TTVSynthesisFailedException, TTVUnsupportedLanguageException
	{
		if(tts == null)
			setupTTS();

		// Check again if the the initialization has been completed. If not, just return
		if(tts == null)
			return;

		if(text == null || text.isEmpty())
			throw new TTVSynthesisFailedException("(No text provided)");

		// Only proceed if able to set language to that specified:
		setNewLanguage(languageCode); // throws TTVUnsupportedLanguageException

		// Instantiate semaphore so we can wait for the job to complete:
		if(ttvJobComplete == null)
			ttvJobComplete = new Semaphore(0);

		// Start synthesis job:
		if(synthesizeToFile(text, filepath) == TextToSpeech.ERROR)
			throw new TTVSynthesisFailedException(text);
		
		// Block thread until synthesis job completes:
		try
		{
			Timber.d("Waiting for job to complete...");
			ttvJobComplete.acquire();
		}
		catch(InterruptedException e)
		{
			Timber.e(e);
		}
		Timber.d("Job completed");
	}
	
	private int synthesizeToFile(String textToSynthesize, String filepath)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP /* = 21 */)
			return synthesizeToFileLollipop(textToSynthesize, filepath);
		else
			return synthesizeToFilePreLollipop(textToSynthesize, filepath);
	}

	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	private int synthesizeToFileLollipop(String textToSynthesize, String filepath)
	{
		return tts.synthesizeToFile(textToSynthesize, null, new File(filepath), textToSynthesize); // Use text to be synthesised as "utterance ID" for job
	}

	@SuppressWarnings("deprecation")
	private int synthesizeToFilePreLollipop(String textToSynthesize, String filepath)
	{
		// Set params of synthesis job by creating a hash table:
		HashMap<String, String> params = new HashMap<String, String>();
		// use text to be synthesised as "utterance ID" for job:
		params.put(TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, textToSynthesize);
		
		// Start synthesis job:
		return tts.synthesizeToFile(textToSynthesize, params, filepath);
	}
	
	/**
	 * Sets the TTS engine's language to the one specified by the provided language code.
	 * 
	 * @param languageCode - the language code in BCP-47 format TODO throws
	 */
	private void setNewLanguage(String languageCode) throws TTVUnsupportedLanguageException
	{
		// only parse code again if it has changed between TTS jobs:
		if(!previousLanguageCode.equals(languageCode) || locale == null)
		{
			// TODO make use of locale parsing stuff which was added in API 21 on supporting devices? 

			// set previous language code to new one:
			previousLanguageCode = languageCode;

			// split language code by "-":
			String[] subtags = languageCode.split("-");

			if(subtags.length > 0)
			{
				// Try to make a locale from this, and let Android reject it later if any component is not valid
				// (it might not support valid languages anyway) :
				locale = new Locale(
							// language:
							subtags[0],
							// country:
							subtags.length > 1 ? subtags[1] : "",
							// variant:
							subtags.length > 2 ? subtags[2] : "");
			}
			else
				// invalid language code (empty after splitting)
				throw new TTVUnsupportedLanguageException(languageCode);

			int result = tts.setLanguage(locale);

			// If the locale is missing, report error
			if(result == TextToSpeech.LANG_MISSING_DATA || result == TextToSpeech.LANG_NOT_SUPPORTED)
				throw new TTVUnsupportedLanguageException(languageCode);
		}
		// else don't need to change locale
	}

	/**
	 * Called when the text-to-speech engine completes the synthesis of a job, identified by the text that the job was synthesising. Unblock the job dispatching
	 * thread so it can process the next job.
	 * 
	 * @param synthesisText - the text that was successfully synthesised.
	 */
	private void onTTSJobCompleted(String synthesisText)
	{
		Timber.d("Completed synthesis for text: " + synthesisText);
		ttvJobComplete.release();
	}

	/**
	 * Release and nullify the text-to-speech engine.
	 */
	public void destroy()
	{
		if(tts != null)
		{
			tts.stop();
			tts.shutdown();
			tts = null;
		}
	}

	/**
	 * Text-to-speech synthesis listener for devices with an API level of 15 or above. API level 15 introduced a more detailed "progress listener" that reports
	 * commencement and failure of a synthesis job rather than just completion.
	 * 
	 * @author benelliott
	 */
	@TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1)
	private class UtteranceListener15Plus extends UtteranceProgressListener
	{

		public UtteranceListener15Plus(TextToSpeech tts)
		{
			tts.setOnUtteranceProgressListener(this);
		}

		@Override
		public void onStart(String utteranceId)
		{
			Timber.d("Started processing text: " + utteranceId);
		}

		@Override
		public void onDone(String utteranceId)
		{
			onTTSJobCompleted(utteranceId);
		}

		@Override
		public void onError(String utteranceId)
		{
			Timber.e("Error processing text: " + utteranceId);
		}

	}

	/**
	 * Text-to-speech synthesis listener for devices with an API level below 15.
	 * 
	 * @author benelliott
	 */
	@SuppressWarnings("deprecation")
	private class UtteranceListenerSub15 implements android.speech.tts.TextToSpeech.OnUtteranceCompletedListener
	{

		public UtteranceListenerSub15(TextToSpeech tts)
		{
			tts.setOnUtteranceCompletedListener(this);
		}

		@Override
		public void onUtteranceCompleted(String utteranceId)
		{
			onTTSJobCompleted(utteranceId);
		}

	}
}
