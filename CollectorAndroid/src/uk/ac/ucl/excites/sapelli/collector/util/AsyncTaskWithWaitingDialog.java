/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.util;

import android.app.ProgressDialog;
import android.content.Context;
import android.os.AsyncTask;

/**
 * @author mstevens
 *
 * @param <Params>
 * @param <Progress>
 * @param <Result>
 */
public abstract class AsyncTaskWithWaitingDialog<Params, Progress, Result> extends AsyncTask<Params, Progress, Result>
{

	private ProgressDialog dialog;		
	
	/**
	 * @param context
	 * @param waitingMsg
	 */
	public AsyncTaskWithWaitingDialog(Context context, String waitingMsg)
	{
		dialog = new ProgressDialog(context);
		dialog.setMessage(waitingMsg);
		dialog.setCancelable(false);
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
