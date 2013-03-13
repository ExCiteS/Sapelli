/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.media.AudioRecorder;
import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.FileHelpers;
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
		
		setNumColumns(1);
		
		//Adapter & images:
		imageAdapter = new ImageAdapter(super.getContext());
		//	Start rec image:
		if(audioField.getStartRecImageLogicalPath() != null)
			startImage = new FileImage(project, audioField.getStartRecImageLogicalPath());
		else
			startImage = new ResourceImage(R.drawable.start_audio_rec);
		//  Stop rec image:
		if(audioField.getStopRecImageLogicalPath() != null)
			stopImage = new FileImage(project, audioField.getStopRecImageLogicalPath());
		else
			stopImage = new ResourceImage(R.drawable.stop_audio_rec);
		imageAdapter.addImage(startImage); //show start button
		
		// Set image dimensions when view dimensions are known:
		getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				imageAdapter.setImageWidth(LayoutParams.MATCH_PARENT);
				imageAdapter.setImageHeight(getHeight());
				setAdapter(imageAdapter);
				
				getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});
				
		// set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				if(audioRecorder == null)
				{	//Start button clicked
					try
					{
						File dataFolder = new File(controller.getProject().getDataPath()); 
						if(!FileHelpers.createFolder(dataFolder))
							throw new IOException("Unable to create data folder: " + dataFolder.getAbsolutePath());
						
						audioRecorder = new AudioRecorder(dataFolder, audioField.generateNewFilename(controller.getCurrentRecord()));
						audioRecorder.start();
					}
					catch(Exception e)
					{
						Log.e(TAG, "Could not start audio recording.", e);
						controller.audioDone(false);
						return; //!!!
					}
					
					//Switch buttons:
					imageAdapter.clear();
					imageAdapter.addImage(stopImage);
					setAdapter(imageAdapter);
				}
				else
				{	//Stop button clicked
					stopRecording();
					controller.audioDone(true);
				}
			}
		});
	}
	
	private void stopRecording()
	{
		try
		{
			audioRecorder.stop();
			//Log.d(TAG, "audioRecording stopped");
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
