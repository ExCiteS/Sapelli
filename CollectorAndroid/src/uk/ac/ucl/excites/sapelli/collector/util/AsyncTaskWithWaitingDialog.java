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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.lang.ref.WeakReference;

import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Context;
import android.os.AsyncTask;

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

	private WeakReference<C> contextRef;
	private final ProgressDialog dialog;
	
	/**
	 * @param context
	 */
	public AsyncTaskWithWaitingDialog(C context)
	{
		this(context, null);
	}
	
	/**
	 * @param context
	 * @param waitingMsg
	 */
	public AsyncTaskWithWaitingDialog(C context, String waitingMsg)
	{
		this.contextRef = new WeakReference<C>(context);
		
		// Create dialog:
		if(context != null && (!(context instanceof Activity) || !((Activity) context).isFinishing()))
		{
			dialog = new ProgressDialog(context);
			if(waitingMsg != null)
				dialog.setMessage(waitingMsg);
			dialog.setCancelable(false);
		}
		else
			dialog = null;
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
		if(hasDialogAndIsRunning())
			dialog.show();
	}

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onProgressUpdate(java.lang.Object[])
	 */
	@Override
	protected void onProgressUpdate(String... msgs)
	{
		if(hasDialogAndIsRunning() && msgs != null && msgs.length > 0)
			dialog.setMessage(msgs[0]);
	}
	
	@Override
	protected void onPostExecute(Result result)
	{
		if(hasDialogAndIsRunning())
			dialog.dismiss();
	}

}
