package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.Locale;

import android.content.Context;
import android.speech.tts.TextToSpeech;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to speak a given text
 * 
 * @author Michalis Vitos
 *
 */
public class TextToVoice implements TextToSpeech.OnInitListener
{
	public static final Locale DEFAULT_LOCALE = Locale.UK;
	public static final String DEFAULT_UNAVAILABLE_CONTENT = "Content not available";

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
			int result = tts.setLanguage(locale);

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
}
