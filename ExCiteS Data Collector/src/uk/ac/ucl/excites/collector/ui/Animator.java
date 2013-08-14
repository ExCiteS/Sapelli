package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.activities.CollectorActivity;
import android.os.AsyncTask;
import android.view.View;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.ScaleAnimation;

/**
 * AsyncTask Class that blocks the UI thread for {@link Animator#delay} milliseconds and executes the animation to the {@link Animator#view}. After the end of
 * the delay and the animation is over, the {@link Animator#runnableTask} will be run.
 * 
 * @author Michalis Vitos
 * 
 */
public class Animator extends AsyncTask<Void, Void, Void>
{
	private long delay;
	private Runnable taskAfterAnimation;
	private View view;

	public Animator(Runnable taskAfterAnimation, View view)
	{
		this(CollectorActivity.UI_ANIMATION_DELAY, taskAfterAnimation, view);
	}

	public Animator(long delay, Runnable taskAfterAnimation, View view)
	{
		this.delay = delay;
		this.taskAfterAnimation = taskAfterAnimation;
		this.view = view;
	}

	@Override
	protected void onPreExecute()
	{
		// Set the app to wait for UI animation
		CollectorActivity.waiteForUIAnimation(true);

		// Set the alpha level of the object
		AlphaAnimation alpha = new AlphaAnimation((float) 1.0, (float) 0.5);
		alpha.setDuration(delay);

		// Control the scale level of the object
		ScaleAnimation scale = new ScaleAnimation(1, (float) 0.96, 1, (float) 0.96, Animation.RELATIVE_TO_SELF, (float) 0.5, Animation.RELATIVE_TO_SELF,
				(float) 0.5);
		scale.setDuration(delay);

		// Create an animation set
		AnimationSet animationSet = new AnimationSet(true);
		animationSet.addAnimation(alpha);
		animationSet.addAnimation(scale);

		if(delay > 0)
			view.startAnimation(animationSet);
	}

	@Override
	protected Void doInBackground(Void... params)
	{
		if(delay > 0)
		{
			try
			{
				Thread.sleep(delay);
			}
			catch(InterruptedException ignore) {}
		}
		return null;
	}

	@Override
	protected void onPostExecute(Void result)
	{
		// Run the task
		taskAfterAnimation.run();

		// Release the UI
		CollectorActivity.waiteForUIAnimation(false);
	}

}
