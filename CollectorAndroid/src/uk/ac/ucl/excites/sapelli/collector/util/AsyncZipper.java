package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.Zipper;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.content.Context;

/**
 * 
 * @author Michalis Vitos
 *
 */
public class AsyncZipper extends AsyncTaskWithWaitingDialog<Void, Void, Void>
{
	private List<String> paths;
	private String zipDest;

	public AsyncZipper(Context context, String waitingMsg, List<String> paths, String zipDest)
	{
		super(context, waitingMsg);

		this.paths = paths;
		this.zipDest = zipDest;
	}

	@Override
	protected Void doInBackground(Void... params)
	{
		try
		{
			Zipper zipper = new Zipper(paths, zipDest);
			zipper.zip();
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
		return null;
	}
}
