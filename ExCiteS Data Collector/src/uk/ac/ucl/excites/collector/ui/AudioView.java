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
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
import uk.ac.ucl.excites.collector.util.AudioRecorder;
import uk.ac.ucl.excites.collector.util.Constants;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, Michalis
 * 
 */
public class AudioView extends PickerView
{
	static public final int COLUMNS = 2;

	private AudioRecorder audioRecorder;
	private boolean recordingDone;

	public AudioView(Context context)
	{
		super(context);
	}

	public void setAudioView(final Audio audio, final ProjectController controller)
	{
		Project project = controller.getProject();
		recordingDone = false;
		
		setNumColumns(COLUMNS);
		
		//Adapter & images:
		int imageSize = (getWidth() - SPACING) / COLUMNS;
		imageAdapter = new ImageAdapter(super.getContext(), imageSize, imageSize); //the images are squares
		//	Start rec image:
		if(audio.getStartRecImageLogicalPath() != null)
			imageAdapter.addImage(new FileImage(project, audio.getStartRecImageLogicalPath()));
		else
			imageAdapter.addImage(new ResourceImage(R.drawable.record));
		//  Stop rec image:
			if(audio.getStopRecImageLogicalPath() != null)
				imageAdapter.addImage(new FileImage(project, audio.getStopRecImageLogicalPath()));
			else
				imageAdapter.addImage(new ResourceImage(R.drawable.stop));
		setAdapter(imageAdapter);

		// set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				if(position == 0)
				{
					imageAdapter.setInvisible(0);
					setAdapter(imageAdapter);

					File cacheDir = new File(controller.getProject().getDataPath() + "/Audio-Recordings"); //TODO get rid of extension, rename files, remove subfolder 
					if(!cacheDir.exists())
						cacheDir.mkdirs();

					String instanceFolder = cacheDir.toString();

					// Timestamp
					String timestamp = Long.toString(System.currentTimeMillis());

					try
					{
						audioRecorder = new AudioRecorder(instanceFolder, timestamp);
					}
					catch(Exception e)
					{
						e.printStackTrace();
						if(Constants.DEBUG_LOG)
							Log.i(Constants.TAG, e.toString());
					}

					// Start the recording
					try
					{
						audioRecorder.start();
						recordingDone = true;
					}
					catch(IOException e)
					{
						e.printStackTrace();
					}
				}
				if(position == 1)
					// Stop the recording
					try
					{
						if(recordingDone)
							audioRecorder.stop();
						controller.audioDone(recordingDone);
					}
					catch(IOException e)
					{
						e.printStackTrace();
					}
			}
		});
	}

}
