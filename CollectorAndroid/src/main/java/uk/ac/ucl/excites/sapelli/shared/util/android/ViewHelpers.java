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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.app.Activity;
import android.graphics.drawable.Drawable;
import android.os.Build;
import androidx.appcompat.app.AppCompatActivity;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.widget.TextView;

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
			return activity.findViewById(androidx.appcompat.R.id.home);
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
		activity.getTheme().resolveAttribute(androidx.appcompat.R.attr.dialogPreferredPadding, typedValue, true);
		return (int) TypedValue.complexToDimensionPixelSize(typedValue.data, activity.getResources().getDisplayMetrics());
	}

	static public int getStartGravity()
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH /* = 14 */)
			return getStartGravityICS();
		else
			return getStartGravityPreICS();
	}
	
	@SuppressLint("RtlHardcoded")
	static private int getStartGravityPreICS()
	{
		return Gravity.LEFT;
	}
	
	@TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	static private int getStartGravityICS()
	{
		return Gravity.START;
	}
	
	static public int getEndGravity()
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH /* = 14 */)
			return getEndGravityICS();
		else
			return getEndGravityPreICS();
	}
	
	@SuppressLint("RtlHardcoded")
	static private int getEndGravityPreICS()
	{
		return Gravity.RIGHT;
	}
	
	@TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	static private int getEndGravityICS()
	{
		return Gravity.END;
	}
	
	static public void setCompoundDrawablesRelativeWithIntrinsicBounds(TextView textView, int start, int top, int end, int bottom)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1 /* = 17 */)
			setCompoundDrawablesRelativeWithIntrinsicBoundsJBMR1(textView, start, top, end, bottom);
		else
			setCompoundDrawablesRelativeWithIntrinsicBoundsPreJBMR1(textView, start, top, end, bottom);
	}
	
	@SuppressLint("RtlHardcoded")
	static public void setCompoundDrawablesRelativeWithIntrinsicBoundsPreJBMR1(TextView textView, int left, int top, int right, int bottom)
	{
		textView.setCompoundDrawablesWithIntrinsicBounds(left, top, right, bottom);
	}
	
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	static private void setCompoundDrawablesRelativeWithIntrinsicBoundsJBMR1(TextView textView, int start, int top, int end, int bottom)
	{
		textView.setCompoundDrawablesRelativeWithIntrinsicBounds(start, top, end, bottom);
	}
	
	static public void setCompoundDrawablesRelativeWithIntrinsicBounds(TextView textView, Drawable start, Drawable top, Drawable end, Drawable bottom)
	{
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1 /* = 17 */)
			setCompoundDrawablesRelativeWithIntrinsicBoundsJBMR1(textView, start, top, end, bottom);
		else
			setCompoundDrawablesRelativeWithIntrinsicBoundsPreJBMR1(textView, start, top, end, bottom);
	}
	
	@SuppressLint("RtlHardcoded")
	static public void setCompoundDrawablesRelativeWithIntrinsicBoundsPreJBMR1(TextView textView, Drawable left, Drawable top, Drawable right, Drawable bottom)
	{
		textView.setCompoundDrawablesWithIntrinsicBounds(left, top, right, bottom);
	}
	
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	static private void setCompoundDrawablesRelativeWithIntrinsicBoundsJBMR1(TextView textView, Drawable start, Drawable top, Drawable end, Drawable bottom)
	{
		textView.setCompoundDrawablesRelativeWithIntrinsicBounds(start, top, end, bottom);
	}
	
}
