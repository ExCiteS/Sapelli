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

package uk.ac.ucl.excites.sapelli.collector.ui.animation;

import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.content.Context;
import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.AnimationSet;
import android.view.animation.CycleInterpolator;
import android.view.animation.TranslateAnimation;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public final class ViewAnimator
{
	
	private static final int DIRECTION_RIGHT = 0;
	private static final int DIRECTION_LEFT = 1;
	private static final int DIRECTION_DOWN = 2;
	private static final int DIRECTION_UP = 3;
	
	
	private static final boolean isHorizontal(int direction)
	{
		return direction < DIRECTION_DOWN;
	}
	
	private static final boolean isVertical(int direction)
	{
		return direction > DIRECTION_LEFT;
	}

	/**
	 * Slide on the right the previous and the next views
	 * 
	 * @param context
	 * @param previousView
	 * @param nextView
	 * @param duration
	 */
	public static void SlideRight(Context context, View previousView, View nextView, int duration)
	{
		Slide(context, previousView, nextView, duration, DIRECTION_RIGHT);
	}

	/**
	 * Slide on the left the previous and the next views
	 * 
	 * @param context
	 * @param previousView
	 * @param nextView
	 * @param duration
	 */
	public static void SlideLeft(Context context, View previousView, View nextView, int duration)
	{
		Slide(context, previousView, nextView, duration, DIRECTION_LEFT);
	}
	
	/**
	 * Slide Down the previous and the next views
	 * 
	 * @param context
	 * @param previousView
	 * @param nextView
	 * @param duration
	 */
	public static void slideDown(Context context, View previousView, View nextView, int duration)
	{
		Slide(context, previousView, nextView, duration, DIRECTION_DOWN);
	}

	/**
	 * Slide Up the previous and the next views
	 * 
	 * @param context
	 * @param previousView
	 * @param nextView
	 * @param duration
	 */
	public static void slideUp(Context context, View previousView, View nextView, int duration)
	{
		Slide(context, previousView, nextView, duration, DIRECTION_UP);
	}

	private static void Slide(Context context, View previousView, View nextView, int duration, int direction)
	{
		int screenWidth = ScreenMetrics.GetScreenWidth(context);

		// Slide previousView:
		if(previousView != null)
		{
			TranslateAnimation previousAnimation = new TranslateAnimation(	0,
																			(isHorizontal(direction) ? (direction == DIRECTION_RIGHT ? 1 : -1) : 0) * screenWidth,
																			0,
																			(isVertical(direction) ? (direction == DIRECTION_DOWN ? 1 : -1) : 0) * screenWidth);
			previousAnimation.setDuration(duration);

			// Create an animation set
			AnimationSet previousSet = new AnimationSet(true);
			previousSet.addAnimation(previousAnimation);

			previousView.startAnimation(previousSet);
		}

		// Slide nextView:
		TranslateAnimation nextAnimation = new TranslateAnimation(	(isHorizontal(direction) ? (direction == DIRECTION_RIGHT ? -1 : 1) : 0) * screenWidth,
																	0,
																	(isVertical(direction) ? (direction == DIRECTION_DOWN ? -1 : 1) : 0) * screenWidth,
																	0);
		nextAnimation.setDuration(duration);
		// Give a minimal offset to separate the two animations
		nextAnimation.setStartOffset(10);

		// Create an animation set
		AnimationSet nextSet = new AnimationSet(true);
		nextSet.addAnimation(nextAnimation);
		nextView.startAnimation(nextSet);
	}
	
	public static void alphaAnimation(View view)
	{
		AlphaAnimation alpha = new AlphaAnimation((float) 1.0, (float) 0.5);
		alpha.setDuration(400);

		// Create an animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.addAnimation(alpha);

		view.startAnimation(animationSet);
	}

	public static void shakeAnimation(Context context, View view)
	{
		// Create the animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.setInterpolator(new CycleInterpolator(3)); // Repeat 3 times

		// Create movement
		final int horizontalShake = ScreenMetrics.ConvertDipToPx(context, 3);
		TranslateAnimation shake = new TranslateAnimation(0, horizontalShake, 0, 0);
		shake.setDuration(600);

		animationSet.addAnimation(shake);

		view.startAnimation(animationSet);
	}
	
	private ViewAnimator()
	{
		// should never be instantiated
	}
	
}
