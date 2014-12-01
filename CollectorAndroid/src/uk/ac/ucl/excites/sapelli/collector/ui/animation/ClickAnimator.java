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

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import android.os.Handler;
import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationSet;
import android.view.animation.ScaleAnimation;

/**
 * "Button click" animation
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public final class ClickAnimator
{
	
	public static final int DURATION = 400; // ms
	
	/**
	 * @param taskAfterAnimation (may be null)
	 * @param clickView
	 * @param controller pass non-null Controller in order to block UI during animation, when null no UI blocking will happen
	 */
	public static void Animate(Runnable taskAfterAnimation, View clickView, CollectorController controller)
	{
		Animate(DURATION, taskAfterAnimation, clickView, controller);
	}

	/**
	 * @param duration
	 * @param taskAfterAnimation (may be null)
	 * @param clickView
	 * @param controller pass non-null Controller in order to block UI during animation, when null no UI blocking will happen
	 */
	public static void Animate(long duration, final Runnable taskAfterAnimation, View clickView, final CollectorController controller)
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

		// Set up listener for the animation:
		animationSet.setAnimationListener(new AnimationListener()
		{
			@Override
			public void onAnimationStart(Animation animation)
			{
				// Block the UI
				if(controller != null)
					controller.blockUI();
			}

			@Override
			public void onAnimationRepeat(Animation animation) {}

			@Override
			public void onAnimationEnd(Animation animation)
			{
				// Run the task on the main/UI thread
				if(taskAfterAnimation != null)
					new Handler().post(taskAfterAnimation);

				// Unblock the UI
				if(controller != null)
					controller.unblockUI();
			}
		});

		// Run the animation:
		clickView.startAnimation(animationSet);
	}
	
	private ClickAnimator()
	{
		// should never be instantiated
	}
	
}
