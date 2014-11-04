package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.HashMap;
import java.util.Locale;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.media.TTVFailedException;
import android.content.Context;
import android.os.Build;
import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.OnUtteranceCompletedListener;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to synthesise a string to a (WAV) file.
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class TextToVoice  implements TextToSpeech.OnInitListener
{
	public static final Locale DEFAULT_LOCALE = Locale.UK;
	private static final String TAG = "TextToVoice";
	private Context context;
	private Locale locale;
	private TextToSpeech tts;
	private Semaphore ttvInitialised;
	private Semaphore ttvJobComplete;
	

	public TextToVoice(Context context)
	{
		this(context, DEFAULT_LOCALE);
	}

	public TextToVoice(Context context, Locale locale)
	{
		this.context = context;
		this.locale = locale;

		// set up initialisation semaphore so thread blocks waiting for TTS init
		ttvInitialised = new Semaphore(0);
		
		setupTTS();
	}
	
	// TODO annotations
	private void setupTTS() {
		tts = new TextToSpeech(context, this);
		
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1 /* 15 */) {
			Log.d(TAG, ">= 15 build version detected");
			UtteranceListener15Plus utteranceListener = new UtteranceListener15Plus(tts);
		} else {
			Log.d(TAG, "< 15 build version detected");
			UtteranceListenerSub15 utteranceListener = new UtteranceListenerSub15(tts);
		}
		
		try {
	        ttvInitialised.acquire();
        } catch (InterruptedException e) {
	        // TODO Auto-generated catch block
	        e.printStackTrace();
        }
	}

	@Override
	public void onInit(int status)
	{
		if(status == TextToSpeech.SUCCESS)
		{
			int result = TextToSpeech.ERROR;

			if(tts != null) 
			{
				result = tts.setLanguage(locale);

				// If the locale is missing, fall back to the default locale
				if(result == TextToSpeech.LANG_MISSING_DATA || result == TextToSpeech.LANG_NOT_SUPPORTED)
					result = tts.setLanguage(DEFAULT_LOCALE);

				if(result != TextToSpeech.LANG_MISSING_DATA && result != TextToSpeech.LANG_NOT_SUPPORTED)
					ttvInitialised.release();
				
	
			}
			// if null, has probably been destroyed before initialisation completed
		}
	}

	public void processSpeechToFile(String text, String filepath) throws TTVFailedException {
		if (tts == null)
			setupTTS();
		
		if(text == null || "".equals(text))
			throw new TTVFailedException("INVALID TEXT SUPPLIED AS ARGUMENT");
		
		HashMap<String, String> params = new HashMap<String, String>();
		params.put(TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, text);
		
		if (ttvJobComplete == null)
			ttvJobComplete = new Semaphore(0);
		
		if (tts.synthesizeToFile(text, params, filepath) == TextToSpeech.ERROR)
			throw new TTVFailedException(text);
		
		try {
	        ttvJobComplete.acquire();
        } catch (InterruptedException e) {
	        // TODO Auto-generated catch block
	        e.printStackTrace();
        }
	}

	public void destroy()
	{
		if(tts != null)
		{	tts.stop();
			tts.shutdown();
			tts = null;
		}
	}
	
	
	private void onTTSJobCompleted(String synthesisText) {
		Log.d(TAG, "Completed synthesis for text: "+synthesisText);
		ttvJobComplete.release();
	}
	
	// TODO listener for > 15
	private class UtteranceListener15Plus extends UtteranceProgressListener
	{

		public UtteranceListener15Plus(TextToSpeech tts) {
			tts.setOnUtteranceProgressListener(this);
        }
		
		@Override
        public void onStart(String utteranceId) {
	        Log.d(TAG, "Started processing text: "+utteranceId);
        }

		@Override
        public void onDone(String utteranceId) {
	        onTTSJobCompleted(utteranceId);
        }

		@Override
        public void onError(String utteranceId) {
	        Log.e(TAG, "Error processing text: "+utteranceId);	        
        }
		
	}
	
	// TODO listener for < 15
	@SuppressWarnings("deprecation")
    private class UtteranceListenerSub15 implements OnUtteranceCompletedListener {
		
		public UtteranceListenerSub15(TextToSpeech tts) {
			tts.setOnUtteranceCompletedListener(this);
		}

		@Override
        public void onUtteranceCompleted(String utteranceId) {
			onTTSJobCompleted(utteranceId);
        }
		
	}
}
