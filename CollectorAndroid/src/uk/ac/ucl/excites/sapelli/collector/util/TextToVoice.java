package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.HashMap;
import java.util.Locale;

import android.content.Context;
import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.Engine;
import android.speech.tts.UtteranceProgressListener;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to speak a given text
 * 
 * @author Michalis Vitos
 *
 */
public class TextToVoice extends UtteranceProgressListener implements TextToSpeech.OnInitListener
{
	public static final Locale DEFAULT_LOCALE = Locale.UK;
	public static final String DEFAULT_UNAVAILABLE_CONTENT = "Content not available";

	private Context context;
	private Locale locale;
	private TextToSpeech tts;
	private boolean initialised = false;
	private HashMap<String, String> queuedTTS;
	private TextSynthesisCompletedListener completedListener;

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

			if(result != TextToSpeech.LANG_MISSING_DATA && result != TextToSpeech.LANG_NOT_SUPPORTED)
				initialised = true;
		}
	}

	/**
	 * Use the Android TTS (Text-To-Speech) Engine to speak the text
	 * 
	 * @param text
	 */
	public void speak(String text)
	{
		if(tts == null || initialised == false)
		{
			tts = new TextToSpeech(context, this);
			return;
		}

		if(text == null || "".equals(text))
			tts.speak(DEFAULT_UNAVAILABLE_CONTENT, TextToSpeech.QUEUE_FLUSH, null);
		else
			tts.speak(text, TextToSpeech.QUEUE_FLUSH, null);
	}
	
	public int processSpeechToFile(String text, String filepath) {
		if (tts == null || !initialised) {
			tts = new TextToSpeech(context, this);
			return TextToSpeech.ERROR;
		}
		if(text == null || "".equals(text))
			return TextToSpeech.ERROR;
			//return tts.synthesizeToFile(DEFAULT_UNAVAILABLE_CONTENT, null, filename);
		else {
			HashMap<String, String> params = new HashMap<String, String>();
			params.put(Engine.KEY_PARAM_UTTERANCE_ID, text);
			if (queuedTTS == null)
				queuedTTS = new HashMap<String, String>();
			queuedTTS.put(text, filepath);
			tts.setOnUtteranceProgressListener(this);
			return tts.synthesizeToFile(text, params, filepath);
		}
	}

	public void stop()
	{
		if(tts != null)
			tts.stop();
	}

	public void destroy()
	{
		if(tts != null)
		{
			tts.stop();
			tts.shutdown();
			tts = null;
		}
	}
	
	public void setOnTextSynthesisCompletedListener(TextSynthesisCompletedListener completedListener) {
		this.completedListener = completedListener;
	}

	@Override
    public void onDone(String utteranceId) {
	    String filepath = queuedTTS.get(utteranceId);
	    if (filepath != null) {
	    	if (completedListener != null)
	    		completedListener.onTextSynthesisCompleted(utteranceId, filepath);
	    }
    }

	@Override
    public void onStart(String utteranceId) {
	    // TODO Auto-generated method stub
	    
    }

	@Override
    public void onError(String utteranceId) {
	    // TODO Auto-generated method stub
	    
    }
}
