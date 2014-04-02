/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis, mstevens
 * 
 */
public class AndroidAudioUI extends AudioUI<View>
{
	
	static private final String TAG = "AndroidAudioUI";
	
	private AudioView view;
	private File audioFile;
	private AudioRecorder audioRecorder;

	public AndroidAudioUI(AudioField audioField, CollectorController controller, CollectorView collectorView)
	{
		super(audioField, controller, collectorView);
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
	public void cancel()
	{
		if(audioRecorder != null)
			stopRecording();
		audioFile = null;
	}

	@Override
	public View getPlatformView(boolean onPage, CollectorRecord record)
	{
		//TODO onPage view
		
		if(view == null)
			view = new AudioView(((CollectorView) collectorUI).getContext());
		
		// Update view:
		//	Make start button visible if more recordings can still be added:
		view.setStartVisibility(showCreateButton());
		
		return view;
	}
	
	public class AudioView extends PickerView implements AdapterView.OnItemClickListener
	{
		
		static private final int BUTTON_INDEX_START = 0;
		static private final int BUTTON_INDEX_STOP = 1;
		
		private int buttonHeight;
		private int buttonPadding;
		private int buttonBackColor;
		
		public AudioView(Context context)
		{
			super(context);

			// UI set-up:
			setBackgroundColor(Color.BLACK);
			int spacingPx = collectorUI.getSpacingPx();
			setHorizontalSpacing(spacingPx);
			setVerticalSpacing(spacingPx);

			// Columns:
			setNumColumns(1);

			// Button size, padding & background colour:
			this.buttonHeight = ((CollectorView) collectorUI).getIconHeightPx(2, controller.getControlsState().isAnyButtonShown());
			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
			this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);
			
			// Adapter & button images:
			pickerAdapter = new PickerAdapter(super.getContext());
			// Start rec button:
			Item startButton = null;
			File startRecImageFile = controller.getProject().getImageFile(field.getStartRecImageRelativePath());
			if(FileHelpers.isReadableFile(startRecImageFile))
				startButton = new FileImageItem(startRecImageFile);
			else
				startButton = new ResourceImageItem(getContext().getResources(), R.drawable.start_audio_rec);
			startButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			addButton(startButton); // show start button
			// Stop rec button:
			Item stopButton = null;
			File stopRecImageFile = controller.getProject().getImageFile(field.getStopRecImageRelativePath());
			if(FileHelpers.isReadableFile(stopRecImageFile))
				stopButton = new FileImageItem(stopRecImageFile);
			else
				stopButton = new ResourceImageItem(getContext().getResources(), R.drawable.stop_audio_rec);
			stopButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			addButton(stopButton); // show stop button

			// Set click listener
			setOnItemClickListener(this);
		}
		
		private void addButton(Item button)
		{
			button.setWidthPx(LayoutParams.MATCH_PARENT);
			button.setHeightPx(buttonHeight);
			button.setPaddingPx(buttonPadding);
			button.setBackgroundColor(buttonBackColor);
			pickerAdapter.addItem(button);
		}
		
		@Override
		public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
		{
			// Task to perform after animation has finished:
			Runnable action = new Runnable()
			{
				public void run()
				{
					switch(position)
					{
						case BUTTON_INDEX_START:
						{
							if(field.isUseNativeApp())
								((CollectorView) collectorUI).getActivity().startAudioRecorderApp(AndroidAudioUI.this);
							else if(startRecording())
								view.setStartVisibility(false);
							break;
						}
						case BUTTON_INDEX_STOP:
						{
							if(audioRecorder == null)
								mediaDone(null, true); // "stop" means "skip" because we are not yet recording
							else
							{	// "stop" really means stop recording
								stopRecording();
								mediaDone(audioFile, true);
							}
							break;
						}
					}
				}
			};

			// Execute the "press" animation if allowed, then perform the action: 
			if(controller.getCurrentForm().isAnimation())
				(new PressAnimator(action, v, (CollectorView) collectorUI)).execute(); //execute animation and the action afterwards
			else
				action.run(); //perform task now (animation is disabled)
		}
		
		public void setStartVisibility(boolean visible)
		{
			pickerAdapter.getItem(BUTTON_INDEX_START).setVisibility(visible);
			setAdapter(pickerAdapter); //this does not seem to be needed on Android 4.x, but it is needed on v2.3.x
		}
		
	}

}
