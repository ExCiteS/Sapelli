package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collect.R;
import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.ImageButton;

/**
 * An Activity with FullScreen and disabled buttons (volume, back etc)
 * 
 * @author Michalis Vitos
 * 
 */
public class FullscreenActivity extends Activity
{

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		// Set to FullScreen
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
				WindowManager.LayoutParams.FLAG_FULLSCREEN);
		// Set the layout to the canvas
		setContentView(R.layout.activity_fullscreen_canvas);

		// Navigation Buttons
		Bundle mBundle = getIntent().getExtras();
		boolean navigationBack = mBundle.getBoolean("navigationBack");

		final ImageButton backButton = (ImageButton) findViewById(R.id.activity_fullscreen_canvas_Back_Button);
		
		if (navigationBack)
		{
			backButton.setOnClickListener(new View.OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					// TODO go back
					finish();
				}
			});
		} else
		{
			backButton.setVisibility(View.GONE);
		}

	}

	@Override
	protected void onPause()
	{
		super.onPause();
	}

	@Override
	protected void onResume()
	{
		super.onResume();
	}

	/*
	 * Disable some keys
	 */
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event)
	{
		switch (keyCode)
		{
		case KeyEvent.KEYCODE_BACK:
			return true;
		case KeyEvent.KEYCODE_DPAD_RIGHT:
			return true;
		case KeyEvent.KEYCODE_DPAD_LEFT:
			return true;
		case KeyEvent.KEYCODE_VOLUME_DOWN:
			return true;
		case KeyEvent.KEYCODE_VOLUME_UP:
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	/*
	 * Disable some keys
	 */
	@Override
	public boolean onKeyUp(int keyCode, KeyEvent event)
	{
		switch (keyCode)
		{
		case KeyEvent.KEYCODE_VOLUME_DOWN:
			return true;
		case KeyEvent.KEYCODE_VOLUME_UP:
			return true;
		}
		return super.onKeyUp(keyCode, event);
	}
}
