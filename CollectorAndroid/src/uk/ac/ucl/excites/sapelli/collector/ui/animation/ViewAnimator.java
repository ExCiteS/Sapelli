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
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationSet;
import android.view.animation.CycleInterpolator;
import android.view.animation.ScaleAnimation;
import android.view.animation.TranslateAnimation;

/**
 * Library of different animations that can be applied to Views
 * 
 * @author Michalis Vitos, mstevens
 */
public class ViewAnimator
{
	
	public static final int DEFAULT_CLICK_DURATION = 400; // ms

	/**
	 * @param viewToAnimate
	 * @param animationSet
	 * @param onAnimationStart code to run (on the main/UI thread) when the animation starts (may be null)
	 * @param onAnimationEnd code to run (on the main/UI thread) when the animation ends (may be null)
	 */
	static protected void Animate(View viewToAnimate, AnimationSet animationSet, final Runnable onAnimationStart, final Runnable onAnimationEnd)
	{
		// Set up listener for the animation:
		if(onAnimationStart != null || onAnimationEnd != null)
			animationSet.setAnimationListener(new AnimationListener()
			{
				@Override
				public void onAnimationStart(Animation animation)
				{
					if(onAnimationStart != null)
						// Run the task on the main/UI thread
						new Handler(Looper.getMainLooper()).post(onAnimationStart); // TODO is this really the UI thread? see javadoc on Handler()
				}
	
				@Override
				public void onAnimationRepeat(Animation animation)
				{
					// not used
				}
	
				@Override
				public void onAnimationEnd(Animation animation)
				{
					if(onAnimationEnd != null)
						// Run the task on the main/UI thread
						new Handler(Looper.getMainLooper()).post(onAnimationEnd); // TODO is this really the UI thread? see javadoc on Handler()
				}
			});

		// Run the animation:
		viewToAnimate.startAnimation(animationSet);
	}
	
	/**
	 * "Button click" animation
	 * 
	 * @param clickedView
	 * @param onAnimationStart code to run (on the main/UI thread) when the animation starts (may be null)
	 * @param onAnimationEnd code to run (on the main/UI thread) when the animation ends (may be null)
	 */
	public static void Click(View clickedView, Runnable onAnimationStart, Runnable onAnimationEnd)
	{
		Click(DEFAULT_CLICK_DURATION, clickedView, onAnimationStart, onAnimationEnd);
	}

	/**
	 * "Button click" animation
	 * 
	 * @param duration
	 * @param clickedView
	 * @param onAnimationStart code to run (on the main/UI thread) when the animation starts (may be null)
	 * @param onAnimationEnd code to run (on the main/UI thread) when the animation ends (may be null)
	 */
	public static void Click(long duration, View clickedView, Runnable onAnimationStart, Runnable onAnimationEnd)
	{
		// Set the alpha level of the object
		AlphaAnimation alpha = new AlphaAnimation((float) 1.0, (float) 0.5);
		alpha.setDuration(duration);

		// Control the scale level of the object
		ScaleAnimation scale = new ScaleAnimation(1, (float) 0.96, 1, (float) 0.96, Animation.RELATIVE_TO_SELF, (float) 0.5, Animation.RELATIVE_TO_SELF, (float) 0.5);
		scale.setDuration(duration);

		// Create an animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.addAnimation(alpha);
		animationSet.addAnimation(scale);

		// Run the animation:
		Animate(clickedView, animationSet, onAnimationStart, onAnimationEnd);
	}

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
	 * @param previousView the view to slide away (may be null)
	 * @param nextView the view to slide in (should *never* be null)
	 * @param duration
	 */
	public static void SlideRight(View previousView, View nextView, int duration)
	{
		Slide(previousView, nextView, duration, DIRECTION_RIGHT);
	}

	/**
	 * Slide on the left the previous and the next views
	 * 
	 * @param previousView the view to slide away (may be null)
	 * @param nextView the view to slide in (should *never* be null)
	 * @param duration
	 */
	public static void SlideLeft(View previousView, View nextView, int duration)
	{
		Slide(previousView, nextView, duration, DIRECTION_LEFT);
	}
	
	/**
	 * Slide Down the previous and the next views
	 * 
	 * @param previousView the view to slide away (may be null)
	 * @param nextView the view to slide in (should *never* be null)
	 * @param duration
	 */
	public static void SlideDown(View previousView, View nextView, int duration)
	{
		Slide(previousView, nextView, duration, DIRECTION_DOWN);
	}

	/**
	 * Slide Up the previous and the next views
	 * 
	 * @param previousView the view to slide away (may be null)
	 * @param nextView the view to slide in (should *never* be null)
	 * @param duration
	 */
	public static void SlideUp(View previousView, View nextView, int duration)
	{
		Slide(previousView, nextView, duration, DIRECTION_UP);
	}

	/**
	 * @param previousView the view to slide away (may be null)
	 * @param nextView the view to slide in (should *never* be null)
	 * @param duration
	 * @param direction
	 */
	private static void Slide(View previousView, View nextView, int duration, int direction)
	{
		int screenWidth = ScreenMetrics.GetScreenWidth(nextView.getContext());

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
			
			// Run the animation:
			Animate(previousView, previousSet, null, null);
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
		
		// Run the animation:
		Animate(nextView, nextSet, null, null);
	}
	
	/**
	 * @param viewToAnimate
	 * @param onAnimationStart code to run (on the main/UI thread) when the animation starts (may be null)
	 * @param onAnimationEnd code to run (on the main/UI thread) when the animation ends (may be null)
	 */
	public static void Alpha(View viewToAnimate, Runnable onAnimationStart, Runnable onAnimationEnd)
	{
		AlphaAnimation alpha = new AlphaAnimation((float) 1.0, (float) 0.5);
		alpha.setDuration(400);

		// Create an animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.addAnimation(alpha);

		// Run the animation:
		Animate(viewToAnimate, animationSet, onAnimationStart, onAnimationEnd);
	}

	/**
	 * @param viewToAnimate
	 * @param onAnimationStart code to run (on the main/UI thread) when the animation starts (may be null)
	 * @param onAnimationEnd code to run (on the main/UI thread) when the animation ends (may be null)
	 */
	public static void Shake(View viewToAnimate, Runnable onAnimationStart, Runnable onAnimationEnd)
	{
		// Create the animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.setInterpolator(new CycleInterpolator(3)); // Repeat 3 times

		// Create movement
		TranslateAnimation shake = new TranslateAnimation(0, ScreenMetrics.ConvertDipToPx(viewToAnimate.getContext(), 3), 0, 0);
		shake.setDuration(600);
		animationSet.addAnimation(shake);

		// Run the animation:
		Animate(viewToAnimate, animationSet, onAnimationStart, onAnimationEnd);
	}
	
	private ViewAnimator()
	{
		// no need to instantiate this
	}

}
