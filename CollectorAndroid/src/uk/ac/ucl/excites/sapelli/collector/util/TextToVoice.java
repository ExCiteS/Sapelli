package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;

import android.content.Context;
import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.Engine;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;

/**
 * Class that uses the Android TTS (Text-To-Speech) Engine to speak a given text or to process
 * its synthesis into a file.
 * 
 * @author Michalis Vitos, benelliott
 *
 */
public class TextToVoice extends UtteranceProgressListener implements TextToSpeech.OnInitListener
{
	public static final Locale DEFAULT_LOCALE = Locale.UK;
	public static final String DEFAULT_UNAVAILABLE_CONTENT = "Content not available";
	private static final String TAG = "TextToVoice";
	private Context context;
	private Locale locale;
	private TextToSpeech tts;
	private boolean initialised = false;
	private ArrayList<TtsJob> queuedTtsJobs;
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

	/**
	 * Calling this method QUEUES the provided text for synthesis into the provided filepath. The actual
	 * synthesis is performed asynchronously, so classes interested in being notified when the 
	 * synthesis is complete should implement TextSynthesisCompletedListener and register themselves with this
	 * class.
	 * @param text - the text to synthesise
	 * @param filepath - the path of the file to which the text should be stored
	 * @return
	 */
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
			if (queuedTtsJobs == null)
				queuedTtsJobs = new ArrayList<TtsJob>();
			synchronized(queuedTtsJobs) {
				queuedTtsJobs.add(new TtsJob(text, filepath));
			}
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

	/**
	 * When a synthesis job has been completed in-order, notify any listeners. Note that this
	 * method enforces the order in which the jobs were added to the queue when calling the listener - if the
	 * second job finishes before the first then the listener will not be told of this until the first job has 
	 * also completed. 
	 */
	@Override
	public void onDone(String utteranceId) {
		// lock queue since this callback may be called by another thread:
		synchronized(queuedTtsJobs) {
			for (int i = 0; i < queuedTtsJobs.size(); i++) {
				// look for job in queue
				// if job with matching text found... 
				if (queuedTtsJobs.get(i).text.equals(utteranceId)) {
					Log.d(TAG,"Found queued job for "+utteranceId+" at index "+i);
					// mark job as completed:
					queuedTtsJobs.get(i).completed = true;
					// if job is at front of queue...
					if (i == 0) {
						// get its filepath
						String filepath = queuedTtsJobs.get(i).filepath;
						if (filepath != null) {
							if (completedListener != null)
								// send it to the completedListener:
								completedListener.onTextSynthesisCompleted(utteranceId, filepath);
						}
						// take it off the queue:
						queuedTtsJobs.remove(i); //TODO multiple items with same text?
					}
					// otherwise, just mark it as completed so that audio isn't played out of order

					// if the new first job is marked as completed (because it was
					// completed before the head of the queue), run this method again for that text:
					if (queuedTtsJobs.get(0).completed)
						onDone(queuedTtsJobs.get(0).text);
					// this process will repeat so long as there are contiguous items which are completed

					// stop looking through the queue as soon as matching text is found:
					break;
				}
			}
		}
	}

	@Override
	public void onStart(String utteranceId) {
		// nothing need be done here yet
	}

	@Override
	public void onError(String utteranceId) {
		// nothing need be done here yet	    
	}

	private class TtsJob {

		private String text;
		private String filepath;
		private boolean completed = false;

		TtsJob(String text, String filepath) {
			this.text = text; 
			this.filepath = filepath;
		}
	}
}
