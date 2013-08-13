/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.media.AudioRecorder;
import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.ui.picker.ImageFileItem;
import uk.ac.ucl.excites.collector.ui.picker.Item;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.collector.ui.picker.ImageResourceItem;
import uk.ac.ucl.excites.util.FileHelpers;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis, mstevens
 * 
 */
public class AudioView extends PickerView implements FieldView
{

	static private final String TAG = "AudioView";

	private File audioFile;
	private AudioRecorder audioRecorder;
	private Item startImage;
	private Item stopImage;

	public AudioView(Context context)
	{
		super(context);
	}

	@Override
	public void initialise(final ProjectController controller, Field field)
	{
		final AudioField audioField = (AudioField) field;
		Project project = controller.getProject();

		// Get audioFile:
		try
		{
			audioFile = audioField.getNewTempFile(controller.getCurrentRecord());
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could get audio file.", e);
			controller.mediaDone(null);
			return; // !!!
		}

		// Columns:
		setNumColumns(1);

		// Adapter & images:
		pickerAdapter = new PickerAdapter(super.getContext());
		// Start rec image:
		File startRecImageFile = project.getImageFile(audioField.getStartRecImageRelativePath());
		if(FileHelpers.isReadableFile(startRecImageFile))
			startImage = new ImageFileItem(startRecImageFile);
		else
			startImage = new ImageResourceItem(R.drawable.start_audio_rec);
		// Stop rec image:
		File stopRecImageFile = project.getImageFile(audioField.getStopRecImageRelativePath());
		if(FileHelpers.isReadableFile(stopRecImageFile))
			stopImage = new ImageFileItem(stopRecImageFile);
		else
			stopImage = new ImageResourceItem(R.drawable.stop_audio_rec);
		pickerAdapter.addItem(startImage); // show start button
		pickerAdapter.addItem(stopImage); // show stop button

		// Set image dimensions when view dimensions are known:
		getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				pickerAdapter.setItemWidth(LayoutParams.MATCH_PARENT);
				pickerAdapter.setItemHeight((getHeight() - PickerView.SPACING) / pickerAdapter.getCount());
				setAdapter(pickerAdapter);

				getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});

		// Set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				if(position == 1) // crossed microphone
				{
					if(audioRecorder == null)
					{
						controller.goForward(true);
					}

					else
					{ // Stop button clicked
						stopRecording();
						controller.mediaDone(audioFile);
					}
				}
				else
				{
					try
					{
						audioRecorder = new AudioRecorder(audioFile);
						audioRecorder.start();
					}
					catch(Exception e)
					{
						Log.e(TAG, "Could not start audio recording.", e);
						controller.mediaDone(null);
						return; // !!!
					}
					// Switch buttons:
					pickerAdapter.makeInvisible(0);
					setAdapter(pickerAdapter);
				}
			}
		});
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
		{
			stopRecording();
		}
	}

	@Override
	public View getView()
	{
		return this;
	}

}
