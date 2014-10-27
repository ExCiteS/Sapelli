package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.HashMap;
import java.util.Locale;

import android.content.Context;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to speak a given text or to process
 * its synthesis into a file.
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class TextToVoice implements TextToSpeech.OnInitListener
{
	public static final Locale DEFAULT_LOCALE = Locale.UK;
	public static final String DEFAULT_UNAVAILABLE_CONTENT = "Content not available";
	private static final String TAG = "TextToVoice";
	private Context context;
	private Locale locale;
	private TextToSpeech tts;
	private boolean initialised = false;

	public TextToVoice(Context context)
	{
		this(context, DEFAULT_LOCALE);
	}

	public TextToVoice(Context context, Locale locale)
	{
		this.context = context;
		this.locale = locale;

		// Set up tts
		tts = new TextToSpeech(context, this);
	}

	@Override
	public void onInit(int status)
	{
		if(status == TextToSpeech.SUCCESS)
		{
			int result = TextToSpeech.ERROR;

			if(tts != null)
				result = tts.setLanguage(locale);

			// If the locale is missing, fall back to the default locale
			if(result == TextToSpeech.LANG_MISSING_DATA || result == TextToSpeech.LANG_NOT_SUPPORTED)
				result = tts.setLanguage(DEFAULT_LOCALE);

			if(result != TextToSpeech.LANG_MISSING_DATA && result != TextToSpeech.LANG_NOT_SUPPORTED) {
				initialised = true;
				Log.d(TAG,"TTS initialised successfully.");
			} else
				Log.e(TAG,"Failure when trying to initialise TTS.");
		}
	}

	/**
	 * Use the Android TTS (Text-To-Speech) Engine to speak the text
	 * 
	 * @param text
	 */
	public void speak(String text)
	{
		if (tts == null) {
			tts = new TextToSpeech(context, this);
			return;
		}
		if (!initialised)
			return;
		
		if(text == null || "".equals(text))
			tts.speak(DEFAULT_UNAVAILABLE_CONTENT, TextToSpeech.QUEUE_FLUSH, null);
		else
			tts.speak(text, TextToSpeech.QUEUE_FLUSH, null);
	}

	public int processSpeechToFile(String text, String filepath) {
		if (tts == null) {
			tts = new TextToSpeech(context, this);
			return TextToSpeech.ERROR;
		}
		if (!initialised)
			return TextToSpeech.ERROR;
		
		if(text == null || "".equals(text))
			return TextToSpeech.ERROR;
		
		HashMap<String, String> params = new HashMap<String, String>();
		params.put(TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, text);
		return tts.synthesizeToFile(text, params, filepath);
	}

	public void destroy()
	{
		if(tts != null)
		{	tts.stop();
			tts.shutdown();
			tts = null;
			initialised = false;
		}
	}
	
	public void setOnUtteranceProgressListener(UtteranceProgressListener listener) {
		tts.setOnUtteranceProgressListener(listener);
	}	
}
