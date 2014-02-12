/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.activities;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;

/**
 * Abstract super class for our activities.
 * 
 * Provides dialog methods.
 * 
 * @author mstevens
 *
 */
public abstract class BaseActivity extends Activity
{
	
	protected CollectorApp app;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
	}
	
	/**
	 * Show dialog with error message
	 * 
	 * @param message
	 */
	public void showErrorDialog(String message, final boolean exitOnOK)
	{
		new AlertDialog.Builder(this).setTitle("Error").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				if(exitOnOK)
					finish();
			}
		}).create().show();
	}
	
	/**
	 * Show dialog with error message
	 * 
	 * @param message
	 */
	public void showErrorDialog(String message, final boolean exitOnOK, final Runnable task)
	{
		new AlertDialog.Builder(this).setTitle("Error").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				task.run();
				if(exitOnOK)
					finish();
			}
		}).create().show();
	}

	/**
	 * Show dialog with warning message
	 * 
	 * @param message
	 */
	public void showWarningDialog(String message)
	{
		new AlertDialog.Builder(this).setTitle("Warning").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton) { /*does nothing*/ }
		}).create().show();
	}
	
	/**
	 * Show dialog with info message
	 * 
	 * @param message
	 */
	public void showInfoDialog(String message)
	{
		showInfoDialog("Info", message);
	}
	
	/**
	 * Show dialog with info message
	 * 
	 * @param title
	 * @param message
	 */
	public void showInfoDialog(String title, String message)
	{
		new AlertDialog.Builder(this).setTitle(title).setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton) { /*does nothing*/ }
		}).create().show();
	}
	
}
