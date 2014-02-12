/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.animation;

import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.ScaleAnimation;

/**
 * "Button press" animaton
 * 
 * @author Michalis Vitos, mstevens
 *
 */
public class PressAnimator extends Animator
{

	public static final int DURATION = 400; // ms
	
	public PressAnimator(Runnable taskAfterAnimation, View animateView, View blockView)
	{
		this(DURATION, taskAfterAnimation, animateView, blockView);
	}
	
	public PressAnimator(long duration, Runnable taskAfterAnimation, View animateView, View blockView)
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
