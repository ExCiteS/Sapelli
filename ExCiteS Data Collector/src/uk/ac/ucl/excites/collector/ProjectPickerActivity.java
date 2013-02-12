package uk.ac.ucl.excites.collector;

import uk.ac.ucl.excites.storage.db.DataStorageAccess;
import android.app.Activity;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

public class ProjectPickerActivity extends Activity
{

	
	
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		setContentView(R.layout.activity_projectpicker);

	//	DataStorageAccess dbInstance = new uk.ac.ucl.excites.storage.db.DataStorageAccess(); 
	}

	
	public void removeProject(){

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
}
