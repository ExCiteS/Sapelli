/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.activities;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
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
	
	private static final int HIDE_BUTTON = -1;
	static private final boolean DEFAULT_FINISH_ON_DIALOG_OK = false;
	static private final boolean DEFAULT_FINISH_ON_DIALOG_CANCEL = false;
	
	protected CollectorApp app;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		this.app = (CollectorApp) getApplication();
	}
	
	public void showOKDialog(int titleId, int messageId)
	{
		showDialog(getString(titleId), getString(messageId), android.R.string.ok, DEFAULT_FINISH_ON_DIALOG_OK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, int messageId)
	{
		showDialog(title, getString(messageId), android.R.string.ok, DEFAULT_FINISH_ON_DIALOG_OK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(int titleId, String message)
	{
		showDialog(getString(titleId), message, android.R.string.ok, DEFAULT_FINISH_ON_DIALOG_OK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, String message)
	{
		showDialog(title, message, android.R.string.ok, DEFAULT_FINISH_ON_DIALOG_OK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(int titleId, int messageId, boolean finishOnOK)
	{
		showDialog(getString(titleId), getString(messageId), android.R.string.ok, finishOnOK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, int messageId, boolean finishOnOK)
	{
		showDialog(title, getString(messageId), android.R.string.ok, finishOnOK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(int titleId, String message, boolean finishOnOK)
	{
		showDialog(getString(titleId), message, android.R.string.ok, finishOnOK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, String message, boolean finishOnOK)
	{
		showDialog(title, message, android.R.string.ok, finishOnOK, null, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(int titleId, int messageId, boolean finishOnOK, Runnable okTask)
	{
		showDialog(getString(titleId), getString(messageId), android.R.string.ok, finishOnOK, okTask, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, int messageId, boolean finishOnOK, Runnable okTask)
	{
		showDialog(title, getString(messageId), android.R.string.ok, finishOnOK, okTask, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(int titleId, String message, boolean finishOnOK, Runnable okTask)
	{
		showDialog(getString(titleId), message, android.R.string.ok, finishOnOK, okTask, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKDialog(String title, String message, boolean finishOnOK, Runnable okTask)
	{
		showDialog(title, message, android.R.string.ok, finishOnOK, okTask, HIDE_BUTTON, DEFAULT_FINISH_ON_DIALOG_CANCEL);
	}
	
	public void showOKCancelDialog(int titleId, int messageId, boolean finishOnOK, Runnable okTask, boolean finishOnCancel)
	{
		showDialog(getString(titleId), getString(messageId), android.R.string.ok, finishOnOK, okTask, android.R.string.cancel, finishOnCancel);
	}

	public void showOKCancelDialog(String title, int messageId, boolean finishOnOK, Runnable okTask, boolean finishOnCancel)
	{
		showDialog(title, getString(messageId), android.R.string.ok, finishOnOK, okTask, android.R.string.cancel, finishOnCancel);
	}
	
	public void showOKCancelDialog(int titleId, String message, boolean finishOnOK, Runnable okTask, boolean finishOnCancel)
	{
		showDialog(getString(titleId), message, android.R.string.ok, finishOnOK, okTask, android.R.string.cancel, finishOnCancel);
	}
	
	public void showOKCancelDialog(String title, String message, boolean finishOnOK, Runnable okTask, boolean finishOnCancel)
	{
		showDialog(title, message, android.R.string.ok, finishOnOK, okTask, android.R.string.cancel, finishOnCancel);
	}
	
	public void showYesNoDialog(int titleId, int messageId, boolean finishOnYes, Runnable yesTask, boolean finishOnNo)
	{
		showDialog(getString(titleId), getString(messageId), R.string.yes, finishOnYes, yesTask, R.string.no, finishOnNo);
	}

	public void showYesNoDialog(String title, int messageId, boolean finishOnYes, Runnable yesTask, boolean finishOnNo)
	{
		showDialog(title, getString(messageId), R.string.yes, finishOnYes, yesTask, R.string.no, finishOnNo);
	}
	
	public void showYesNoDialog(int titleId, String message, boolean finishOnYes, Runnable yesTask, boolean finishOnNo)
	{
		showDialog(getString(titleId), message, R.string.yes, finishOnYes, yesTask, R.string.no, finishOnNo);
	}
	
	public void showYesNoDialog(String title, String message, boolean finishOnYes, Runnable yesTask, boolean finishOnNo)
	{
		showDialog(title, message, R.string.yes, finishOnYes, yesTask, R.string.no, finishOnNo);
	}
	
	/**
	 * @param title
	 * @param message
	 * @param postiveButtonId
	 * @param finishOnPositive
	 * @param positiveTask
	 * @param negativeButtonId
	 * @param finishOnNegative
	 */
	private void showDialog(String title, String message, int postiveButtonId, final boolean finishOnPositive, final Runnable positiveTask, int negativeButtonId, boolean finishOnNegative)
	{
		// Builder:
		AlertDialog.Builder bldr = new AlertDialog.Builder(this);
		
		// set title:
		bldr.setTitle(title);
		// set message:
		bldr.setMessage(message);
		// set positive button:
		bldr.setPositiveButton(postiveButtonId, finishOnPositive || positiveTask != null ? new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				if(positiveTask != null)
					positiveTask.run();
				if(finishOnPositive)
					finish();
			}
		} : null);
		// set negative button:
		if(negativeButtonId != -1)
			bldr.setNegativeButton(negativeButtonId, finishOnNegative ? new DialogInterface.OnClickListener()
			{
				public void onClick(DialogInterface dialog, int whichButton)
				{
					finish();
				}
			} : null);
		// create & show
		bldr.create().show();
	}
	
	/**
	 * Show dialog with error message
	 * 
	 * @param message
	 */
	public void showErrorDialog(String message, boolean finish)
	{
		showOKDialog(R.string.error, message, finish);
	}

	/**
	 * Show dialog with error message
	 * 
	 * @param messageId
	 */
	public void showErrorDialog(int messageId, boolean finish)
	{
		showOKDialog(R.string.error, messageId, finish);
	}
	
	/**
	 * Show dialog with warning message
	 * 
	 * @param message
	 */
	public void showWarningDialog(String message)
	{
		showOKDialog(R.string.warning, message, false);
	}
	
	/**
	 * Show dialog with warning message
	 * 
	 * @param messageId
	 */
	public void showWarningDialog(int messageId)
	{
		showOKDialog(R.string.warning, messageId, false);
	}
	
	/**
	 * Show dialog with info message
	 * 
	 * @param message
	 */
	public void showInfoDialog(String message)
	{
		showOKDialog(R.string.info, message, false);
	}
	
	/**
	 * Show dialog with info message
	 * 
	 * @param messageId
	 */
	public void showInfoDialog(int messageId)
	{
		showOKDialog(R.string.info, messageId, false);
	}
	
}
