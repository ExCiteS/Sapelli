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
