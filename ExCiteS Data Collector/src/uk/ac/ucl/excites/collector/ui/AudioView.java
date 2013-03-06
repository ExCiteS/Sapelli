/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Audio;
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
	private int imageSize;
	private boolean recordingDone;

	public AudioView(Context context)
	{
		super(context);

	}

	public void setAudioView(final Audio audio, final ProjectController controller)
	{

		recordingDone = false;
		imageSize = (getWidth() - SPACING) / COLUMNS;
		imageAdapter = new ImageAdapter(super.getContext(), controller.getProject(), imageSize, imageSize);
		if(audio.getRecordingImagePath() != null && audio.getStopImagePath() != null)
		{
			imageAdapter.IconsToDisplay(audio);
		}
		else
		{
			imageAdapter.audioIconsToDisplay();
		}
		setNumColumns(COLUMNS);
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
