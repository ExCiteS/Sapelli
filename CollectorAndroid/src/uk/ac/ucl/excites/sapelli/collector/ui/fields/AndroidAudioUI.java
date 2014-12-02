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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.media.AudioRecorder;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis, mstevens
 * 
 */
public class AndroidAudioUI extends AudioUI<View, CollectorView>
{
	
	static private final String TAG = "AndroidAudioUI";

	private CollectorController controller;
	private AudioView view;
	private File audioFile;
	private AudioRecorder audioRecorder;

	public AndroidAudioUI(AudioField audioField, CollectorController controller, CollectorView collectorView)
	{
		super(audioField, controller, collectorView);

		this.controller = controller;
	}
	
	private boolean startRecording()
	{
		try
		{
			audioFile = field.getNewTempFile(controller.getFileStorageProvider(), controller.getCurrentRecord());
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

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		//TODO onPage view
		//TODO take "enabled" into account
		
		if(view == null)
			view = new AudioView(collectorUI.getContext());
		
		// Update view:
		//	Make start button visible if more recordings can still be added:
		view.setStartVisibility(showCreateButton());
		view.setStopVisibility(isValid(record));
		
		return view;
	}
	
	public class AudioView extends PickerView implements AdapterView.OnItemClickListener
	{
		
		static private final int BUTTON_INDEX_START = 0;
		static private final int BUTTON_INDEX_STOP = 1;
		
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
			this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, collectorUI.getFieldUIPartHeightPx(2));
			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
			this.buttonBackColor = ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR);
			
			// Adapter & button images:
			// Start rec button:
			Item startButton = null;
			File startRecImageFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getStartRecImageRelativePath());
			if(FileHelpers.isReadableFile(startRecImageFile))
				startButton = new FileImageItem(startRecImageFile);
			else
				startButton = new ResourceImageItem(getContext().getResources(), R.drawable.start_audio_rec);
			startButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			addButton(startButton); // add start button

			// Stop rec button:
			Item stopButton = null;
			File stopRecImageFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getStopRecImageRelativePath());
			if(FileHelpers.isReadableFile(stopRecImageFile))
				stopButton = new FileImageItem(stopRecImageFile);
			else
				stopButton = new ResourceImageItem(getContext().getResources(), R.drawable.stop_audio_rec);
			stopButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			addButton(stopButton); // add stop button

			// Set click listener
			setOnItemClickListener(this);
		}
		
		private void addButton(Item button)
		{
			button.setPaddingPx(buttonPadding);
			button.setBackgroundColor(buttonBackColor);
			getAdapter().addItem(button);
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
							controller.addLogLine("CLICK_START_RECORDING");
							
							if(field.isUseNativeApp())
								collectorUI.getActivity().startAudioRecorderApp(AndroidAudioUI.this);
							else if(startRecording())
							{
								view.setStartVisibility(false);
								view.setStopVisibility(true);
							}
							break;
						}
						case BUTTON_INDEX_STOP:
						{
							controller.addLogLine("CLICK_STOP_RECORDING");
							
							if(audioRecorder == null)
								mediaDone(null, true); // "stop" means "skip" because we are not yet recording
							else
							{	// "stop" really means stop recording
								stopRecording();
							mediaDone(audioFile, true); // will also call goForward(0 on the controller, even if max is not reached
							}
							break;
						}
					}
				}
			};

			// Perform the click
			controller.clickView(v, action);
		}
		
		public void setStartVisibility(boolean visible)
		{
			PickerAdapter adapter = getAdapter();
			Item startItem = adapter.getItem(BUTTON_INDEX_START);
			if(startItem != null)
				startItem.setVisibility(visible);
			setAdapter(adapter); //this does not seem to be needed on Android 4.x, but it is needed on v2.3.x (TODO test if it is really so)
		}
		
		public void setStopVisibility(boolean visible)
		{
			PickerAdapter adapter = getAdapter();
			Item stopItem = adapter.getItem(BUTTON_INDEX_STOP);
			if(stopItem != null)
				stopItem.setVisibility(visible);
			setAdapter(adapter); // this does not seem to be needed on Android 4.x, but it is needed on v2.3.x (TODO test if it is really so)
		}

	}

}
