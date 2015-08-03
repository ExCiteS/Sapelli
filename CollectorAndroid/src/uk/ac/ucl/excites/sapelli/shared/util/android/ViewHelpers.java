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
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.support.v7.app.AppCompatActivity;
import android.util.TypedValue;
import android.view.View;

/**
 * @author mstevens
 *
 */
public final class ViewHelpers
{

	private ViewHelpers()
	{
	}

	static public void setViewBackground(View view, Drawable background)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN /* = 16 */)
			setViewBackgroundDrawable16AndUp(view, background);
		else
			setViewBackgroundDrawablePre16(view, background);
	}

	@TargetApi(Build.VERSION_CODES.JELLY_BEAN)
	static private void setViewBackgroundDrawable16AndUp(View view, Drawable background)
	{
		view.setBackground(background);
	}

	@SuppressWarnings("deprecation")
	static private void setViewBackgroundDrawablePre16(View view, Drawable background)
	{
		view.setBackgroundDrawable(background);
	}

	/**
	 * Method which returns the View representing the home button ("hamburger") on activities with an Action/Toolbar.
	 * 
	 * TODO this doesn't seem to work (at least not on Lollipop) -> always returns null View
	 * 
	 * @param activity
	 * @return
	 */
	@TargetApi(Build.VERSION_CODES.HONEYCOMB)
	static public View getActionBarHomeView(final Activity activity)
	{
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB && !(activity instanceof AppCompatActivity))
			return activity.findViewById(android.R.id.home);
		else
			return activity.findViewById(android.support.v7.appcompat.R.id.home);
	}
	
	/**
	 * Returns, as pixels, the default/preferred padding on used on (Alert)Dialog contents
	 * 
	 * @param activity
	 * @return
	 */
	static public int getDefaultDialogPaddingPx(Activity activity)
	{
		TypedValue typedValue = new TypedValue(); 
		activity.getTheme().resolveAttribute(android.R.attr.dialogPreferredPadding, typedValue, true);
		return (int) TypedValue.complexToDimensionPixelSize(typedValue.data, activity.getResources().getDisplayMetrics());
	}

}
