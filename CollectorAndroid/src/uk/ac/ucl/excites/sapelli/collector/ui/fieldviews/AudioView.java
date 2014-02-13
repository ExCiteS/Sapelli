/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.collector.ProjectController;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.util.FileHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis, mstevens
 * 
 */
@SuppressLint("ViewConstructor")
public class AudioView extends PickerView implements FieldUI, AdapterView.OnItemClickListener
{

	static private final int BUTTON_INDEX_START = 0;
	static private final int BUTTON_INDEX_STOP = 1;
	
	static private final String TAG = "AudioView";

	private CollectorView collectorView;
	
	private final ProjectController controller;
	private final AudioField audioField;
	
	private File audioFile;
	private AudioRecorder audioRecorder;

	private int buttonHeight;
	private int buttonPadding;
	private int buttonBackColor;

	public AudioView(Context context, CollectorView collectorView, final ProjectController controller, AudioField field)
	{
		super(context);
		this.collectorView = collectorView;
		this.controller = controller;
		this.audioField = field;
		Project project = controller.getProject();

		// UI set-up:
		setBackgroundColor(Color.BLACK);
		int spacingPx = collectorView.getSpacingPx();
		setHorizontalSpacing(spacingPx);
		setVerticalSpacing(spacingPx);

		// Columns:
		setNumColumns(1);

		// Button size, padding & background colour:
		this.buttonHeight = collectorView.getIconHeightPx(2, controller.getButtonsState().isAnyButtonShown());
		this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, ChoiceView.PADDING_DIP);
		this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);
		
		// Adapter & button images:
		pickerAdapter = new PickerAdapter(super.getContext());
		// Start rec button:
		Item startButton = null;
		File startRecImageFile = project.getImageFile(audioField.getStartRecImageRelativePath());
		if(FileHelpers.isReadableFile(startRecImageFile))
			startButton = new FileImageItem(startRecImageFile);
		else
			startButton = new ResourceImageItem(getContext().getResources(), R.drawable.start_audio_rec);
		startButton.setBackgroundColor(ColourHelpers.ParseColour(audioField.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		addButton(startButton); // show start button
		// Stop rec button:
		Item stopButton = null;
		File stopRecImageFile = project.getImageFile(audioField.getStopRecImageRelativePath());
		if(FileHelpers.isReadableFile(stopRecImageFile))
			stopButton = new FileImageItem(stopRecImageFile);
		else
			stopButton = new ResourceImageItem(getContext().getResources(), R.drawable.stop_audio_rec);
		stopButton.setBackgroundColor(ColourHelpers.ParseColour(audioField.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
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
	public void update()
	{
		// Make start button visible
		pickerAdapter.getItem(BUTTON_INDEX_START).setVisibility(true);
		setAdapter(pickerAdapter); //this does not seem to be needed on Android 4.x, but it is needed on v2.3.x
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
						if(startRecording())
						{
							// Switch buttons:
							pickerAdapter.getItem(BUTTON_INDEX_START).setVisibility(false);
							setAdapter(pickerAdapter);
						}
						break;
					}
					case BUTTON_INDEX_STOP:
					{
						if(audioRecorder == null)
							controller.goForward(true);
						else
						{
							stopRecording();
							controller.mediaDone(audioFile);
						}
						break;
					}
				}
			}
		};

		// Execute the "press" animation if allowed, then perform the action: 
		if(controller.getCurrentForm().isAnimation())
			(new PressAnimator(action, v, collectorView)).execute(); //execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)
	}
	
	private boolean startRecording()
	{
		try
		{
			audioFile = audioField.getNewTempFile(controller.getCurrentRecord());
			audioRecorder = new AudioRecorder(audioFile);
			audioRecorder.start();
		}
		catch(IOException ioe)
		{
			Log.e(TAG, "Could get audio file.", ioe);
			controller.mediaDone(null);
			return false;
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not start audio recording.", e);
			controller.mediaDone(null);
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
	}

	@Override
	public Field getField()
	{
		return audioField;
	}

}
