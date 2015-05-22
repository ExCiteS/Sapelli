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

import android.app.ProgressDialog;
import android.content.Context;
import android.os.AsyncTask;

/**
 * @author mstevens
 *
 * @param <Params>
 * @param <Result>
 */
public abstract class AsyncTaskWithWaitingDialog<Params, Result> extends AsyncTask<Params, String, Result>
{

	protected final Context context; 
	private final ProgressDialog dialog;
	
	/**
	 * @param context
	 */
	public AsyncTaskWithWaitingDialog(Context context)
	{
		this(context, null);
	}
	
	/**
	 * @param context
	 * @param waitingMsg
	 */
	public AsyncTaskWithWaitingDialog(Context context, String waitingMsg)
	{
		this.context = context;
		dialog = new ProgressDialog(context);
		if(waitingMsg != null)
			dialog.setMessage(waitingMsg);
		dialog.setCancelable(false);
	}

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onProgressUpdate(java.lang.Object[])
	 */
	@Override
	protected void onProgressUpdate(String... msgs)
	{
		if(msgs != null && msgs.length > 0)
			dialog.setMessage(msgs[0]);
	}

	@Override
	protected void onPreExecute()
	{
		dialog.show();
	}
	
	@Override
	protected void onPostExecute(Result result)
	{
		dialog.dismiss();
	}

}
