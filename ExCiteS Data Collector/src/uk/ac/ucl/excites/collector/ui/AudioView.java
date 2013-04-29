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
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.Image;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
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
	private Image startImage;
	private Image stopImage;

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
		imageAdapter = new ImageAdapter(super.getContext());
		// Start rec image:
		if(audioField.getStartRecImageLogicalPath() != null)
			startImage = new FileImage(project, audioField.getStartRecImageLogicalPath());
		else
			startImage = new ResourceImage(R.drawable.start_audio_rec);
		// Stop rec image:
		if(audioField.getStopRecImageLogicalPath() != null)
			stopImage = new FileImage(project, audioField.getStopRecImageLogicalPath());
		else
			stopImage = new ResourceImage(R.drawable.stop_audio_rec);
		imageAdapter.addImage(startImage); // show start button
		imageAdapter.addImage(stopImage); // show stop button

		// Set image dimensions when view dimensions are known:
		getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				imageAdapter.setImageWidth(LayoutParams.MATCH_PARENT);
				imageAdapter.setImageHeight(getHeight() / imageAdapter.getCount());
				setAdapter(imageAdapter);

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
					imageAdapter.makeInvisible(0);
					setAdapter(imageAdapter);
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
