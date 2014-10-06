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
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageButton;

public class AndroidAudioUI extends AndroidMediaUI<AudioField> {
		
	private boolean recording = false;
	private boolean playing = false;
	
	static private final String TAG = "AndroidAudioUI";
	
	private File audioFile;
	private AudioRecorder audioRecorder;

	public AndroidAudioUI(AudioField field, Controller controller,
            CollectorView collectorUI) {
	    super(field, controller, collectorUI);
    }
	
    @Override
    public void populateCaptureLayout(ViewGroup captureLayout) {
    	// TODO some illustration of audio levels		
    }
	
	@Override
    public void populateReviewLayout(ViewGroup reviewLayout) {
		//TODO make a layout that lets you listen to the new recording
		Context context = reviewLayout.getContext();
		ImageButton playAudioButton = new ImageButton(context);
		playAudioButton.setImageResource(R.drawable.audio_item_svg);
		playAudioButton.setOnClickListener(new OnClickListener(){
			@Override
            public void onClick(View v) {
	            if (playing) {
	            	playing = false;
	            	Log.d("Play audio button","Stop playing");
	            	// TODO
	            } 
	            else {
	            	playing = true;
	            	Log.d("Play audio button","Start playing");
	            	// TODO
	            }
            }
		});
		reviewLayout.addView(playAudioButton);
    }

	@Override
    public boolean onCapture() {
        if (recording) {
        	// stop recording
        	stopRecording();
        	Log.d("AudioUI","Stopped recording");
        	return true;
        } else {
        	// start recording
        	recording = true;
        	startRecording();
        	Log.d("AudioUI","Started recording");
        	// TODO change button image to "stop"
        	return false;
        }
	}

	@Override
    public void onMediaSaved() {
		//TODO
    }

	@Override
    public void onCaptureStarted() {
		//TODO 
    }

	@Override
    public void populateDeleteLayout(ViewGroup deleteLayout, File mediaFile) {
		//TODO
    }

	@Override
    public void finalise() {
		//TODO
    }

	@Override
    public ImageItem getCaptureButton(Context context) {
		ImageItem captureButton = null;
		File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
		if(FileHelpers.isReadableFile(captureImgFile))
			captureButton = new FileImageItem(captureImgFile);
		else
			captureButton = new ResourceImageItem(context.getResources(), R.drawable.start_audio_rec);
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
    }

	@Override
    public List<Item> getMediaItems() {
	    List<File> files = controller.getMediaAttachments();
	    List<Item> items = new ArrayList<Item>();
	    for (File f : files) {
	    	items.add(new FileImageItem(f));
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
			Log.e(TAG, "Could get audio file.", ioe);
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


	
}
