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
import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.ScaleAnimation;

/**
 * "Button click" animation
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class ClickAnimator extends Animator
{

	public static final int DURATION = 400; // ms
	
	/**
	 * @param taskAfterAnimation (may be null)
	 * @param animateView
	 * @param controller when not null controller is used to block entire UI during animation
	 */
	public ClickAnimator(Runnable taskAfterAnimation, View animateView, CollectorController controller)
	{
		this(DURATION, taskAfterAnimation, animateView, controller);
	}
	
	/**
	 * @param duration
	 * @param taskAfterAnimation (may be null)
	 * @param animateView
	 * @param controller when not null controller is used to block entire UI during animation 
	 */
	public ClickAnimator(long duration, Runnable taskAfterAnimation, View animateView, CollectorController controller)
	{
		super(duration, taskAfterAnimation, animateView, controller);
	}
	
	/**
	 * @param taskAfterAnimation (may be null)
	 * @param animateView
	 * @param blockView specific view to block during animation (may be null)
	 */
	public ClickAnimator(Runnable taskAfterAnimation, View animateView, View blockView)
	{
		this(DURATION, taskAfterAnimation, animateView, blockView);
	}
	
	/**
	 * @param duration
	 * @param taskAfterAnimation (may be null)
	 * @param animateView
	 * @param blockView specific view to block during animation (may be null)
	 */
	public ClickAnimator(long duration, Runnable taskAfterAnimation, View animateView, View blockView)
	{
		super(duration, taskAfterAnimation, animateView, blockView);
	}

	@Override
	protected AnimationSet getAnimationSet()
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
		
		return animationSet;
	}

}
