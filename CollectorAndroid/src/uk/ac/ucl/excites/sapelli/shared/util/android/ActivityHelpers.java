package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.annotation.TargetApi;
import android.app.Activity;
import android.app.ActivityManager;
import android.graphics.Bitmap;
import android.os.Build;

/**
 * @author mstevens
 *
 */
public final class ActivityHelpers
{
	
	private ActivityHelpers() {}

	static public void setTaskDescription(Activity activity, String title, Bitmap icon, int backgroundColor)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP /* = 21 */)
			setTaskDescriptionLollipop(activity, title, icon, backgroundColor);
	}

	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	static private void setTaskDescriptionLollipop(Activity activity, String title, Bitmap icon, int backgroundColor)
	{
		activity.setTaskDescription(new ActivityManager.TaskDescription(title, icon, backgroundColor));
	}
	
}
