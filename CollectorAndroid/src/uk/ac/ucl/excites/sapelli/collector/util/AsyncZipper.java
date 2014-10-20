package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.shared.io.Zipper;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;

/**
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class AsyncZipper extends AsyncTaskWithWaitingDialog<Void, Void, Void>
{
	private Context context;
	private List<File> sourceFiles;
	private String zipDest;

	public AsyncZipper(Context context, String waitingMsg, List<File> sourceFiles, String zipDest)
	{
		super(context, waitingMsg);

		this.context = context;
		this.sourceFiles = sourceFiles;
		this.zipDest = zipDest;
	}

	@Override
	protected Void doInBackground(Void... params)
	{
		try
		{
			new Zipper(sourceFiles, zipDest).zip();
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
		return null;
	}

	@Override
	protected void onPostExecute(Void result)
	{
		super.onPostExecute(result);

		// Show Dialog
		new AlertDialog.Builder(context).setTitle(R.string.successful_backup).setMessage(context.getString(R.string.backup_in) + "\r\n" + zipDest)
				.setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int which)
					{
						// continue with delete
					}
				}).create().show();
	}
}
