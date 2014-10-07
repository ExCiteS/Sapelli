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
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

public class AndroidAudioUI extends AndroidMediaUI<AudioField> {
		
	private boolean recording = false;
	private boolean playing = false;
	
	static private final String TAG = "AndroidAudioUI";
	
	private File audioFile;
	private AudioRecorder audioRecorder;
	
	private ViewGroup captureLayout;

	public AndroidAudioUI(AudioField field, Controller controller,
            CollectorView collectorUI) {
	    super(field, controller, collectorUI);
    }

	@Override
    boolean onCapture() {
        if (recording) {
        	// stop recording
        	recording = false;
        	stopRecording();
        	Log.d("AudioUI","Stopped recording");
        	if (captureLayout != null) {
        		captureLayout.removeViewAt(1); // remove spinner
        	}
        	return true; // data has been captured, so return true
        } else {
        	// start recording
        	recording = true;
        	startRecording();
        	Log.d("AudioUI","Started recording");
        	
        	if (captureLayout != null) {
				LinearLayout waitView = new LinearLayout(captureLayout.getContext());
				waitView.setGravity(Gravity.CENTER);
				waitView.addView(new ProgressBar(captureLayout.getContext(), null, android.R.attr.progressBarStyleLarge));
				captureLayout.addView(waitView);
        	}
        	
        	// TODO change button image to "stop"
        	return false; // we have only started capturing data, so return false
        }
	}

	@Override
    void onApprove() {
		if (audioFile != null) {
			// add the file to the field's attachments:
			if (multipleCapturesAllowed) {
				mediaAddedButNotDone(audioFile);
			}
			else {
				mediaDone(audioFile, true);
			}
			audioFile = null;
		}
    }

	@Override
    void onDiscard() {
		//TODO -- currently just does nothing, assuming temp file will be deleted eventually
    }

	@Override
    void populateDeleteLayout(ViewGroup deleteLayout, File mediaFile) {
		//TODO
    }
	
	@Override
    ImageItem getCaptureButton(Context context) {
		ImageItem captureButton = null;
		File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
		if(FileHelpers.isReadableFile(captureImgFile))
			captureButton = new FileImageItem(captureImgFile);
		else
			captureButton = new ResourceImageItem(context.getResources(), R.drawable.audio_item_svg);
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
    }

	@Override
    List<Item> getMediaItems() {
		// TODO make this specific to audio
	    List<File> files = controller.getMediaAttachments();
	    List<Item> items = new ArrayList<Item>();
	    for (File f : files) {
	    	items.add(new AudioItem(f));
	    }
	    return items;
    }

	private boolean startRecording()
	{
		try
		{
			audioFile = field.getNewTempFile(controller.getCurrentRecord());
			audioRecorder = new AudioRecorder(audioFile);
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
		audioFile = null;
	}
	
	
    @Override
    void populateCaptureLayout(ViewGroup captureLayout) {
    	// TODO some illustration of audio levels
    	this.captureLayout = captureLayout;
    	TextView placeholder = new TextView(captureLayout.getContext());
    	placeholder.setText("Levels go here");
    	captureLayout.addView(placeholder);
    }
	
	@Override
    void populateReviewLayout(ViewGroup reviewLayout) {
		// create a button for playing the newly captured audio:
		Context context = reviewLayout.getContext();
				
		// TODO:
//		File approveImgFile = controller.getProject().getImageFile(field.getApproveButtonImageRelativePath());
//		if(FileHelpers.isReadableFile(approveImgFile))
//			approveButton = new FileImageItem(approveImgFile);
//		else
		final ImageItem playAudioButton = new ResourceImageItem(context.getResources(), R.drawable.button_play_audio_svg);
		playAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		
		//final ImageItem stopAudioButton = null;
		// TODO:
//		File approveImgFile = controller.getProject().getImageFile(field.getApproveButtonImageRelativePath());
//		if(FileHelpers.isReadableFile(approveImgFile))
//			approveButton = new FileImageItem(approveImgFile);
//		else
		final ImageItem stopAudioButton = new ResourceImageItem(context.getResources(), R.drawable.button_stop_audio_svg);
		stopAudioButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		
		
		final PickerView buttonPicker = new PickerView(context);
		buttonPicker.setNumColumns(1);
		// TODO rework this:
		buttonPicker.setItemDimensionsPx(
				LayoutParams.MATCH_PARENT,
				collectorUI.getFieldUIPartHeightPx(
						collectorUI.getFieldUIHeightPx() - ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP) - collectorUI.getSpacingPx(),1));
		// height of picker = available UI height - height of bottom control bar
		buttonPicker.setOnItemClickListener(new OnItemClickListener(){

			@Override
            public void onItemClick(AdapterView<?> parent, View view,
                    int position, long id) {
	            // only one button - the play/stop button
				Runnable buttonAction = new Runnable() {
					public void run() {
			            if (playing) {
			            	// if already playing, then stop audio
			            	playing = false;
			            	Log.d("Play audio button","Stop playing");
			            	// present play button to user: 
			            	buttonPicker.getAdapter().clear();
			            	buttonPicker.getAdapter().addItem(playAudioButton);
			            	buttonPicker.getAdapter().notifyDataSetChanged();
			            	// TODO stop audio
			            } 
			            else {
			            	// if not playing, then start playing audio
			            	playing = true;
			            	Log.d("Play audio button","Start playing");
			            	// present stop button to user: 
			            	buttonPicker.getAdapter().clear();
			            	buttonPicker.getAdapter().addItem(stopAudioButton);			            	buttonPicker.getAdapter().notifyDataSetChanged();
			            	buttonPicker.getAdapter().notifyDataSetChanged();
			            	// TODO play audio
			            }
					}
				};	            
				// Execute the "press" animation if allowed, then perform the action: 
				if(controller.getCurrentForm().isClickAnimation())
					(new ClickAnimator(buttonAction, view, collectorUI)).execute(); //execute animation and the action afterwards
				else
					buttonAction.run(); //perform task now (animation is disabled)
            }
		});
		// add button to picker:
		buttonPicker.getAdapter().addItem(playAudioButton);
		// add picker to container:
		reviewLayout.addView(buttonPicker);
    }
}
