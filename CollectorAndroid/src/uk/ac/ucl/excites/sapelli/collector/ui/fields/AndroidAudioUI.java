package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.AudioItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.net.Uri;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ProgressBar;

public class AndroidAudioUI extends AndroidMediaUI<AudioField> {

	private volatile Boolean recording = false;

	static private final String TAG = "AndroidAudioUI";

	private AudioRecorder audioRecorder;
	private AudioReviewPicker audioReviewPicker;

	private ViewGroup captureLayout;

	public AndroidAudioUI(AudioField field, Controller controller,
			CollectorView collectorUI) {
		super(field, controller, collectorUI);
	}

	private boolean startRecording()
	{
		try
		{
			lastCaptureFile = field.getNewTempFile(controller.getCurrentRecord());
			audioRecorder = new AudioRecorder(lastCaptureFile);
			audioRecorder.start();
		}
		catch(IOException ioe)
		{
			Log.e(TAG, "Could not get audio file.", ioe);
			mediaDone(null, false);
			return false;
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not start audio recording.", e);
			mediaDone(null, false);
			return false; // !!!
		}
		return true;		
	}

	private void stopRecording()
	{
		try
		{
			audioRecorder.stop();
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error on stopping audio recording.", e);
		}
		finally
		{
			audioRecorder = null;
		}
	}

	@Override
	protected void cancel()
	{
		if(audioRecorder != null)
			stopRecording();
		lastCaptureFile = null;
	}


	@Override
	void onInitialiseCaptureMode() {
		// nothing to do here 
	}

	@Override
	boolean onCapture() {
		synchronized(recording) {
			if (!recording) {
				// start recording
				startRecording();
				Log.d("AudioUI","Started recording");
				// add spinner:
				if (captureLayout != null) {
					ProgressBar progress = new ProgressBar(captureLayout.getContext(), null, android.R.attr.progressBarStyleLarge);
					captureLayout.addView(progress);
				}
				recording = true;
			} else {
				// stop recording
				stopRecording();
				Log.d("AudioUI","Stopped recording");
				// remove spinner:
				if (captureLayout != null) {
					captureLayout.removeViewAt(0); 
				}
				// a capture has been made so show it for review:
				showCaptureForReview();
				recording = false;
			}
		}
		// always allow other click events after this completes (so recording can be stopped by pressing again):
		return true; 
	}
	
	@Override
	void onDiscard() {
		if (audioReviewPicker != null)
			audioReviewPicker.finalise();
	}

	@Override
	ImageItem generateCaptureButton(Context context) {
		ImageItem captureButton = null;
		if (!recording) {
			// recording hasn't started yet, so present "record" button
			File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
			if(FileHelpers.isReadableFile(captureImgFile))
				// use a custom audio capture image if available
				captureButton = new FileImageItem(captureImgFile);
			else
				// otherwise just use the default resource
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.audio_item_svg);

		}
		else {
			// recording started, so present "stop" button instead
			File stopImgFile = controller.getProject().getImageFile(field.getStopAudioImageRelativePath());
			if(FileHelpers.isReadableFile(stopImgFile))
				captureButton = new FileImageItem(stopImgFile);
			else
				captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg);
		}
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
	}

	@Override
	List<Item> getMediaItems() {
		// TODO: assumes all attachments (regardless of type) are relevant to this field!
		List<File> files = controller.getMediaAttachments();
		List<Item> items = new ArrayList<Item>();
		for (File f : files) {
			items.add(new AudioItem(f)); //TODO will fail if file is a photo -- should change when refactored
		}
		return items;
	}

	@Override
	void populateCaptureLayout(ViewGroup captureLayout) {
		// TODO some illustration of audio levels
		this.captureLayout = captureLayout;
	}

	@Override
	void populateReviewLayout(ViewGroup reviewLayout, File mediaFile) {
		Log.d("AudioUI","Populating review layout...");
		reviewLayout.removeAllViews();
		// create buttons for playing the newly captured audio:
		audioReviewPicker = new AudioReviewPicker(reviewLayout.getContext(), mediaFile);
		// add picker to container:
		reviewLayout.addView(audioReviewPicker);
	}

	@Override
	void finalise() {
		if (audioReviewPicker != null)
			audioReviewPicker.finalise();
	}

	private class AudioReviewPicker extends PickerView implements OnItemClickListener, MediaPlayer.OnCompletionListener {
		
		private MediaPlayer mediaPlayer = new MediaPlayer();
		private Runnable buttonAction;
		private final ImageItem playAudioButton;
		private final ImageItem stopAudioButton;
		private volatile Boolean playing = false;


		public AudioReviewPicker(Context context, File audioFile) {
			super(context);
			
			try {
				Log.d("AudioReviewPicker","Initialising mediaPlayer...");
	            mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
	            mediaPlayer.setDataSource(context, Uri.fromFile(audioFile));
	            mediaPlayer.setOnCompletionListener(this);
            } catch (IOException e) {
	            Log.e(TAG, "Could not play audio file.");
	            e.printStackTrace();
            }
			File playImgFile = controller.getProject().getImageFile(field.getPlayAudioImageRelativePath());
			if(FileHelpers.isReadableFile(playImgFile))
				playAudioButton = new FileImageItem(playImgFile);
			else
				playAudioButton = new ResourceImageItem(context.getResources(), R.drawable.button_play_audio_svg);
			playAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));

			File stopImgFile = controller.getProject().getImageFile(field.getStopAudioImageRelativePath());
			if(FileHelpers.isReadableFile(stopImgFile))
				stopAudioButton = new FileImageItem(stopImgFile);
			else
				stopAudioButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg);
			stopAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));

			setNumColumns(1);

			// TODO rework this:
			setItemDimensionsPx(
					LayoutParams.MATCH_PARENT,
					collectorUI.getFieldUIPartHeightPx(
							collectorUI.getFieldUIHeightPx() - ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP) - collectorUI.getSpacingPx(),1));
			// height of picker = available UI height - height of bottom control bar

			setOnItemClickListener(this);
			
			buttonAction = new Runnable() {
				// only one button - the play/stop button
				public void run() {
					synchronized(playing) {
						if (!playing) {
							// if not playing, then start playing audio
							playAudio();
							playing = true;
						} 
						else {
							// if already playing, then stop audio
							stopAudio();
							playing = false;
						}
					}
					handlingClick.release();
				}
			};

			getAdapter().addItem(playAudioButton);
		}

		@Override
		public void onItemClick(AdapterView<?> parent, View view, int position,
				long id) {
			if (handlingClick.tryAcquire()) {
			// Execute the "press" animation if allowed, then perform the action: 
			if(controller.getCurrentForm().isClickAnimation())
				(new ClickAnimator(buttonAction, view, collectorUI)).execute(); //execute animation and the action afterwards
			else
				buttonAction.run(); //perform task now (animation is disabled)
			}
		}
		
		private void stopAudio() {
			Log.d("AudioReviewPicker","stop");
			// stop the audio:
			mediaPlayer.stop();
			// present the play button to the user:
			getAdapter().clear();
			getAdapter().addItem(playAudioButton);
			getAdapter().notifyDataSetChanged();
		}
		
		private void playAudio() {
			Log.d("AudioReviewPicker","play");
			// play the audio: 
			try {
				mediaPlayer.prepare();
				mediaPlayer.start();
            } catch (IOException e) {
	            Log.e(TAG, "Could not play audio file.");
	            e.printStackTrace();
            }
			// present the play button to the user:
			getAdapter().clear();
			getAdapter().addItem(stopAudioButton);
			getAdapter().notifyDataSetChanged();
		}
		
		private void finalise() {
			Log.d("AudioReviewPicker","finalise");
			synchronized(playing) {
				if (mediaPlayer != null)
					mediaPlayer.release();
				mediaPlayer = null;
				playing = false;
			}
		}

		@Override
        public void onCompletion(MediaPlayer mp) {
	        // called when the media player finishes playing its media file
			Log.d("AudioReviewPicker","on completion");
			synchronized(playing) {
				// go from PlaybackCompleted to Stopped
				stopAudio();
				playing = false;
			}
        }
	}
}
