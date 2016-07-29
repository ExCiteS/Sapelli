/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.lang.ref.WeakReference;
import android.annotation.TargetApi;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.os.AsyncTask;
import android.os.Build;
import android.util.Log;

/**
 * @author mstevens
 *
 * @param <C> Context type 
 * @param <Params>
 * @param <Result>
 * 
 * @see http://stackoverflow.com/a/18665887/1084488
 */
public abstract class AsyncTaskWithWaitingDialog<C extends Context, Params, Result> extends AsyncTask<Params, String, Result>
{

	private final WeakReference<C> contextRef;
	private final String waitingMsg;
	private final boolean cancelable;
	private ProgressDialog dialog;
	
	/**
	 * @param context
	 */
	public AsyncTaskWithWaitingDialog(C context)
	{
		this(context, false);
	}

	/**
	 * @param context
	 * @param cancelable
	 */
	public AsyncTaskWithWaitingDialog(C context, boolean cancelable)
	{
		this(context, null, cancelable);
	}
	
	/**
	 * @param context
	 * @param waitingMsgId
	 */
	public AsyncTaskWithWaitingDialog(C context, int waitingMsgId)
	{
		this(context, waitingMsgId, false);
	}
	
	/**
	 * @param context
	 * @param waitingMsgId
	 * @param cancelable
	 */
	public AsyncTaskWithWaitingDialog(C context, int waitingMsgId, boolean cancelable)
	{
		this(context, context.getString(waitingMsgId), cancelable);
	}
	
	/**
	 * @param context
	 * @param waitingMsg
	 */
	public AsyncTaskWithWaitingDialog(C context, String waitingMsg)
	{
		this(context, waitingMsg, false);
	}
	
	/**
	 * @param context
	 * @param waitingMsg
	 * @param cancelable
	 */
	public AsyncTaskWithWaitingDialog(C context, String waitingMsg, boolean cancelable)
	{
		this.contextRef = new WeakReference<C>(context);
		this.waitingMsg = waitingMsg;
		this.cancelable = cancelable;
	}
	
	protected C getContext()
	{
		C context = contextRef.get();
		if(context == null || (context instanceof Activity && ((Activity) context).isFinishing()))
			return null;
		else
			return context;
	}
	
	protected boolean isRunning()
	{
		return getContext() != null;
	}
	
	protected boolean hasDialogAndIsRunning()
	{
		return isRunning() && dialog != null;
	}
	
	@Override
	protected void onPreExecute()
	{
		// Create dialog:
		Context context = getContext();
		if(context != null)
		{
			dialog = new ProgressDialog(context);
			if(waitingMsg != null) // set waiting msg if one was given
				dialog.setMessage(waitingMsg);
			dialog.setCancelable(cancelable);
			if(cancelable)
			{
				dialog.setOnCancelListener(new OnCancelListener()
				{
					@Override
					public void onCancel(DialogInterface dialog)
					{
						cancel(true);
					}
				});
				dialog.setButton(DialogInterface.BUTTON_NEGATIVE, context.getString(android.R.string.cancel), new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int which)
					{
						cancel(true);
					}
				});
			}
			// Do *not* show dialog yet! It will be shown upon first call to publishProgress()! (see doInBackground())
		}
		else
			dialog = null;
	}
	
	/* (non-Javadoc)
	 * @see android.os.AsyncTask#doInBackground(java.lang.Object[])
	 */
	@Override
	protected final Result doInBackground(@SuppressWarnings("unchecked") Params... params)
	{
		// Open waiting dialog:
		publishProgress(); // no arguments --> will open the dialog without setting a new message
		
		// Do the actual work & return result:
		return runInBackground(params);
	}
	
	/**
	 * @param params
	 * @return
	 * @see android.os.AsyncTask#doInBackground(java.lang.Object[])
	 */
	protected abstract Result runInBackground(@SuppressWarnings("unchecked") Params... params);

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onProgressUpdate(java.lang.Object[])
	 */
	@Override
	protected void onProgressUpdate(String... msgs)
	{
		try
		{
			if(hasDialogAndIsRunning())
			{
				// Open dialog if needed:
				if(!dialog.isShowing())
					dialog.show();
				// Set new message if there is one:
				if(msgs != null && msgs.length > 0)
					dialog.setMessage(msgs[0]);
			}
		}
		catch(Exception e)
		{
			Log.e(getClass().getName(), "Error in onProgressUpdate()", e);
		}
	}
	
	/**
	 * @see http://stackoverflow.com/a/5102572/1084488
	 * @see https://github.com/ExCiteS/Sapelli/issues/42
	 */
	protected void dismisDialog()
	{
		try
		{
			if(hasDialogAndIsRunning() && dialog.isShowing())
			{
				dialog.dismiss(); // may throw IllegalArgumentException (e.g. when activity finishes prematurely)
			}
		}
		catch(final Exception e)
		{
			Log.e(getClass().getName(), "Error in dismisDialog()", e);
		}
		finally
		{
			dialog = null;
		}
	}
	
	@Override
	protected void onPostExecute(Result result)
	{
		dismisDialog();
	}

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onCancelled(java.lang.Object)
	 */
	@TargetApi(Build.VERSION_CODES.HONEYCOMB)
	@Override
	protected void onCancelled(Result result)
	{
		dismisDialog();
	}

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onCancelled()
	 */
	@Override
	protected void onCancelled()
	{
		dismisDialog();
	}

}
