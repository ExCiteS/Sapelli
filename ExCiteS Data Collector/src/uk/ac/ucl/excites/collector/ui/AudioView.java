/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.model.Audio;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.Image;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
import uk.ac.ucl.excites.collector.util.AudioRecorder;
import uk.ac.ucl.excites.collector.util.Cancelable;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis, mstevens
 * 
 */
public class AudioView extends PickerView implements Cancelable
{

	static private final String TAG = "AudioView";

	private AudioRecorder audioRecorder;
	private Image startImage;
	private Image stopImage;
	
	public AudioView(Context context)
	{
		super(context);
	}

	public void setAudioView(final Audio audio, final ProjectController controller)
	{
		Project project = controller.getProject();		
		
		setNumColumns(1);
		
		//Adapter & images:
		imageAdapter = new ImageAdapter(super.getContext());
		//	Start rec image:
		if(audio.getStartRecImageLogicalPath() != null)
			startImage = new FileImage(project, audio.getStartRecImageLogicalPath());
		else
			startImage = new ResourceImage(R.drawable.record);
		//  Stop rec image:
		if(audio.getStopRecImageLogicalPath() != null)
			stopImage = new FileImage(project, audio.getStopRecImageLogicalPath());
		else
			stopImage = new ResourceImage(R.drawable.stop);
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
						File cacheDir = new File(controller.getProject().getDataPath() + "/Audio-Recordings"); //TODO get rid of extension, rename files, remove subfolder 
						if(!cacheDir.exists())
							cacheDir.mkdirs();
						String instanceFolder = cacheDir.toString();

						// Timestamp
						String timestamp = Long.toString(System.currentTimeMillis());
						
						audioRecorder = new AudioRecorder(instanceFolder, timestamp);
						audioRecorder.start();
					}
					catch(IOException e)
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
					try
					{
						audioRecorder.stop();
					}
					catch(IOException e)
					{
						Log.e(TAG, "Error on stopping audio recording.", e);
					}
					finally
					{
						audioRecorder = null;
						controller.audioDone(true);
					}
				}
			}
		});
	}

	@Override
	public void cancel()
	{
		if(audioRecorder != null)
		{
			try
			{
				audioRecorder.stop();
				//Log.d(TAG, "audioRecording stopped");
			}
			catch(IOException e)
			{
				Log.e(TAG, "Error on stopping audio recording.", e);
			}
			finally
			{
				audioRecorder = null;
			}
		}
	}

}
