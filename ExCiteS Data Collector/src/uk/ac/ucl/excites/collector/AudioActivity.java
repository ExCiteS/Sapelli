package uk.ac.ucl.excites.collector;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.collector.ui.FullscreenActivity;
import uk.ac.ucl.excites.collector.util.AudioRecorder;
import uk.ac.ucl.excites.collector.util.Constants;
import android.content.Context;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

public class AudioActivity extends FullscreenActivity
{

	private AudioRecorder mAudioRecorder;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Insert the layout for the recording
		LayoutInflater mLayoutInflater = (LayoutInflater) this
				.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		ViewGroup canvas = (ViewGroup) findViewById(R.id.activity_fullscreen_canvas_LinerarLayout);
		mLayoutInflater.inflate(R.layout.activity_recording, canvas);

		// Set up Buttons
		final ImageButton backButton = (ImageButton) findViewById(R.id.activity_fullscreen_canvas_Back_Button);
		final ImageButton startRecording = (ImageButton) findViewById(R.id.activity_recording_Start);
		final ImageButton stopRecording = (ImageButton) findViewById(R.id.activity_recording_Stop);
		stopRecording.setEnabled(false);

		// Start Recording
		startRecording.setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View v)
			{

				// TODO Get the folder name
				File mSDcard = Environment.getExternalStorageDirectory();
				File cacheDir = new File(mSDcard.getAbsolutePath() + "/ExCiteS-Test-Recordings");
				if (!cacheDir.exists())
					cacheDir.mkdirs();

				String mInstanceFolder = cacheDir.toString();

				// Timestamp
				String mTimestamp = Long.toString(System.currentTimeMillis());

				try
				{
					mAudioRecorder = new AudioRecorder(mInstanceFolder, mTimestamp);

				} catch (Exception e)
				{
					e.printStackTrace();
					if (Constants.DEBUG_LOG)
						Log.i(Constants.TAG, e.toString());
				}

				backButton.setEnabled(false);
				startRecording.setEnabled(false);

				// Start the recording
				try
				{
					mAudioRecorder.start();

				} catch (IOException e)
				{
					e.printStackTrace();
				}

				stopRecording.setEnabled(true);
			}
		});

		// Stop Recording
		stopRecording.setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View v)
			{

				startRecording.setEnabled(false);
				backButton.setEnabled(false);

				// Stop the recording
				try
				{
					mAudioRecorder.stop();
				} catch (IOException e)
				{
					e.printStackTrace();
				}

				// TODO Move on
				finish();
			}
		});
	}
}
