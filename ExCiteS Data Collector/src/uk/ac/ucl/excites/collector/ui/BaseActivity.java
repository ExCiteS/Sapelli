/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

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

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
	}
	
	/**
	 * dialog shown when erroneous user interaction is detected
	 * 
	 * @param message
	 * @return the dialog
	 */
	protected AlertDialog errorDialog(String message, final boolean exitOnOK)
	{
		AlertDialog error = new AlertDialog.Builder(this).setTitle("Error").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				if(exitOnOK)
					finish();
			}
		}).create();
		return error;
	}
	
	/**
	 * dialog showing warnings
	 * 
	 * @param message
	 * @return the dialog
	 */
	protected AlertDialog warningDialog(String message)
	{
		AlertDialog warning = new AlertDialog.Builder(this).setTitle("Warning").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton) { /*does nothing*/ }
		}).create();
		return warning;
	}
	
	/**
	 * dialog showing info
	 * 
	 * @param message
	 * @return the dialog
	 */
	protected AlertDialog infoDialog(String message)
	{
		AlertDialog warning = new AlertDialog.Builder(this).setTitle("Info").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton) { /*does nothing*/ }
		}).create();
		return warning;
	}
	
}
