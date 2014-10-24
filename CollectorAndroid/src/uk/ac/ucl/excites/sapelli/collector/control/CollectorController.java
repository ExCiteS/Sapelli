/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationListener;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationSensor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.util.AudioPlayer;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.LocationUtils;
import uk.ac.ucl.excites.sapelli.collector.util.TextSynthesisCompletedListener;
import uk.ac.ucl.excites.sapelli.collector.util.TextToVoice;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.SystemClock;
import android.speech.tts.TextToSpeech;
import android.util.Log;

import com.crashlytics.android.Crashlytics;

/**
 * @author mstevens, Michalis Vitos, Julia
 * 
 */
public class CollectorController extends Controller implements LocationListener, OrientationListener, TextSynthesisCompletedListener
{

	// STATICS-------------------------------------------------------
	public static final String TAG = "CollectorController";
	public static final int LOCATION_LISTENER_UPDATE_MIN_TIME_MS = 15 * 1000; // 15 seconds
	public static final int LOCATION_LISTENER_UPDATE_MIN_DISTANCE_M = 5; // 5 meters

	// DYNAMICS------------------------------------------------------
	public CollectorActivity activity;

	private LocationManager locationManager;
	private Location currentBestLocation = null;
	private OrientationSensor orientationSensor;
	private long deviceIDHash;

	private AudioPlayer audioPlayer;
	private TextToVoice textToVoice;

	public CollectorController(Project project, CollectorView collectorView, ProjectStore projectStore, RecordStore recordStore, FileStorageProvider fileStorageProvider, CollectorActivity activity)
	{
		super(project, collectorView, projectStore, recordStore, fileStorageProvider);
		this.activity = activity;

		// Set/change last running project:
		Crashlytics.setString(CollectorApp.PROPERTY_LAST_PROJECT, project.toString(true));
		System.getProperties().setProperty(CollectorApp.PROPERTY_LAST_PROJECT, project.toString(true));

		// Get Device ID (as a CRC32 hash):
		try
		{
			deviceIDHash = DeviceID.GetInstance(activity).getIDAsCRC32Hash();
		}
		catch(IllegalStateException ise)
		{
			activity.showErrorDialog("DeviceID has not be initialised!", true);
		}
	}
	
	@Override
	public uk.ac.ucl.excites.sapelli.storage.types.Location getCurrentBestLocation()
	{
		return LocationUtils.getSapelliLocation(currentBestLocation); // passing null returns null
	}

	@Override
	protected void saveRecordAndAttachments()
	{
		super.saveRecordAndAttachments(); // !!!

		// Also print the record on Android Log:
		if(currFormSession.form.isProducesRecords())
		{
			Log.d(TAG, "Stored record:");
			Log.d(TAG, currFormSession.record.toString());
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
		File file = getTemporaryFile();
		// QUEUE the text for synthesis (which is conducted asynchronously)
		if (textToVoice.processSpeechToFile(text, file.getAbsolutePath()) != TextToSpeech.SUCCESS) {
			Log.e(TAG,"Error when trying to save synthesised speech to disk.");
		}

	}
	
	/**
	 * When a text synthesis job has been completed, add it to the play queue.
	 */
	@Override
    public void onTextSynthesisCompleted(String text, String filepath) {
		File soundFile = new File(filepath);
		playSound(soundFile, true, true);
		addLogLine("TEXT_TO_VOICE", text);
    }

	
	/**
	 * @return a temporary file into which synthesised speech can be saved, with the expectation that it will be
	 * deleted after being played.
	 */
	private File getTemporaryFile() {
		File tempFolder = this.getFileStorageProvider().getTempFolder(true);
		int filename = 0;
		File tempFile;
		while ((tempFile = new File(tempFolder, Integer.toString(filename))).exists()) {
			filename++;
		}
		Log.d(TAG,"Returning temporary file: "+tempFile.getAbsolutePath());
		return tempFile;
	}

	/**
	 * Use Media Player to play a given audio file + logging
	 * 
	 * @param soundFile
	 */
	public void audioToVoice(File soundFile)
	{
		Log.d(TAG,"Audio to voice: "+soundFile.getAbsolutePath());
		// Play the audio in a queued fashion but do not delete the file afterwards:
		playSound(soundFile, true, false);
		addLogLine("AUDIO_TO_VOICE", soundFile.getAbsolutePath());
	}

	public void stopAudioFeedback()
	{
		// Stop the Media Player
		if(audioPlayer != null)
			audioPlayer.stopQueue();

		// Stop the Android TTS (Text-To-Speech) Engine
		if(textToVoice != null)
			textToVoice.stop();
	}

	/**
	 * Play a sound immediately.
	 */
	@Override
	protected void playSound(File soundFile) {
		playSound(soundFile, false);
	}
	
	/**
	 * Play a sound, with it optionally being queued (i.e. if another queued sound is
	 * currently being played, wait for that to finish before playing this one). 
	 * @param soundFile
	 * @param queueSound
	 */
	protected void playSound(File soundFile, boolean queueSound) {
		playSound(soundFile, queueSound, false);
	}
	
	/**
	 * Play a sound, with it optionally being queued and optionally being deleted after playing
	 * (useful if the file is only meant to be played once, e.g. a synthesised piece of text).
	 * @param soundFile
	 * @param queueSound
	 * @param deleteAfterPlaying
	 */
	protected void playSound(File soundFile, boolean queueSound, boolean deleteAfterPlaying)
	{
		if(audioPlayer == null)
			audioPlayer = new AudioPlayer(activity.getBaseContext());
		if (queueSound) {
			audioPlayer.enqueueAndPlay(soundFile, deleteAfterPlaying);
		} else {
			audioPlayer.playImmediate(soundFile);
		}
	}

	@Override
	protected void startOrientationListener()
	{
		if(orientationSensor == null)
			orientationSensor = new OrientationSensor(activity);
		orientationSensor.start(this); // start listening for orientation updates
	}
	
	@Override
	protected void stopOrientationListener()
	{
		if(orientationSensor != null)
			orientationSensor.stop();
	}
	
	public void onOrientationChanged(Orientation orientation)
	{
		if(orientation == null)
			return;
		// Stop listening for orientation updates:
		stopOrientationListener();
		// Use orientation:
		if(getCurrentField() instanceof OrientationField) // !!!
		{
			((OrientationField) getCurrentField()).storeOrientation(currFormSession.record, orientation);
			goForward(false);
		}
	}

	protected void startLocationListener(List<LocationField> locFields)
	{
		if(locFields.isEmpty())
			return;
		// Get locationmanager:
		if(locationManager == null)
			locationManager = (LocationManager) activity.getSystemService(Context.LOCATION_SERVICE);
		// Determine which provider(s) we need:
		Set<String> providers = new HashSet<String>();
		for(LocationField lf : locFields)
			providers.addAll(LocationUtils.getProvider(locationManager, lf));
		// Start listening to each provider:
		for(String provider : providers)
		{
			locationManager.requestLocationUpdates(provider, LOCATION_LISTENER_UPDATE_MIN_TIME_MS, LOCATION_LISTENER_UPDATE_MIN_DISTANCE_M, this);
			// Test if provider is active:
			if(!locationManager.isProviderEnabled(provider))
			{
				activity.showOKDialog(R.string.app_name, activity.getString(R.string.enableLocationProvider, provider), true, new Runnable()
				{ 	// TODO how will non/illiterates deal with this, and what if the Sapelli launcher is used (settings screen will be inaccessible)?
					@Override
					public void run()
					{
						activity.startActivityForResult(new Intent(android.provider.Settings.ACTION_LOCATION_SOURCE_SETTINGS), 0);
					}
				});
			}
		}
	}

	protected void stopLocationListener()
	{
		if(locationManager != null)
			locationManager.removeUpdates(this);
	}

	@Override
	public synchronized void onLocationChanged(Location location)
	{
		if(LocationUtils.isBetterLocation(location, currentBestLocation))
		{
			currentBestLocation = location;
			// check if we can/need to use the location now:
			if(getCurrentField() instanceof LocationField)
			{ // user is currently waiting for a location for the currFormSession.currField
				LocationField lf = (LocationField) getCurrentField();
				// try to store location:
				if(lf.storeLocation(currFormSession.record, LocationUtils.getSapelliLocation(location)))
				{ // location successfully stored:
					if(currFormSession.form.getLocationFields(true).isEmpty())
						stopLocationListener(); // there are no locationfields with startWithForm=true (so there is no reason to keep listening for locations)
					goForward(false); // continue (will leave waiting screen & stop the timeout timer)
				}
				// else (location was not accepted): do nothing (keep listening for a better location)
			}
		}
	}

	@Override
	public void onProviderDisabled(String provider)
	{
		// does nothing for now
	}

	@Override
	public void onProviderEnabled(String provider)
	{
		// does nothing for now
	}

	@Override
	public void onStatusChanged(String provider, int status, Bundle extras)
	{
		// does nothing for now
	}

	@Override
	protected void setupKeyPressTrigger(Trigger trigger)
	{
		activity.setupKeyPressTrigger(trigger);
	}

	@Override
	protected void setupTimerTrigger(Trigger trigger)
	{
		activity.setupTimerTrigger(trigger);
	}

	@Override
	protected void disableKeyPressTrigger(Trigger trigger)
	{
		activity.disableKeyPressTrigger(trigger);
	}

	@Override
	protected void disableTimerTrigger(Trigger trigger)
	{
		activity.disableTimerTrigger(trigger);
	}

	@Override
	protected void vibrate(int durationMS)
	{
		DeviceControl.vibrate(activity, durationMS);
	}

	@Override
	protected void exitApp()
	{
		activity.finish();
	}

	public void enableAudioFeedback()
	{
		Log.d(TAG,"Enable audio feedback...");
		// Check if any of the forms has audio feedback enabled
		for(Form f : project.getForms())
		{
			final AudioFeedback audioFeedback = f.getAudioFeedback();

			if(audioFeedback != null)
			{
				switch(audioFeedback)
				{
				case LONG_CLICK:
				case SEQUENTIAL:

					// Enable Audio Files Feedback: nothing to do, audioPlayer instance will be creaded when playSound() is called

					// Enable TTS Audio Feedback
					if(textToVoice == null) {
						textToVoice = new TextToVoice(activity.getBaseContext(), activity.getResources().getConfiguration().locale);
						textToVoice.setOnTextSynthesisCompletedListener(this);
					}
					break;

				case NONE:
				}
			}
		}
	}

	public void disableAudioFeedback()
	{
		// Release the Media Player
		if(audioPlayer != null)
			audioPlayer.destroy();

		// Release the Android TTS (Text-To-Speech) Engine
		if(textToVoice != null)
			textToVoice.destroy();
	}

	@Override
	protected void showError(String errorMsg, boolean exit)
	{
		activity.showErrorDialog(errorMsg, exit);
	}

	@Override
	protected long getDeviceID()
	{
		return deviceIDHash;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.control.Controller#getElapsedMillis()
	 */
	@Override
	protected long getElapsedMillis()
	{
		return SystemClock.elapsedRealtime();
	}

}
