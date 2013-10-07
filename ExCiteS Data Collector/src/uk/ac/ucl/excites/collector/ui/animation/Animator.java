package uk.ac.ucl.excites.collector.ui.animation;

import android.os.AsyncTask;
import android.view.View;
import android.view.animation.AnimationSet;

/**
 * AsyncTask Class that blocks the UI thread for {@link Animator#duration} milliseconds and executes the animation to the {@link Animator#view}. After the end of
 * the duration and the animation is over, the {@link Animator#runnableTask} will be run.
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public abstract class Animator extends AsyncTask<Void, Void, Void>
{
	
	protected long duration;
	private Runnable taskAfterAnimation;
	private View animateView;
	private View blockView;

	/**
	 * @param duration  running time of the animation (in ms)
	 * @param taskAfterAnimation  Runnable that is to be executed after the animation is finished
	 * @param animateView  View to which the animation will be applied
	 * @param blockView  View which is to be disabled while the animation is running (can be null)
	 */
	public Animator(long duration, Runnable taskAfterAnimation, View animateView, View blockView)
	{
		this.duration = duration;
		this.taskAfterAnimation = taskAfterAnimation;
		this.animateView = animateView;
		this.blockView = blockView;
	}

	@Override
	protected void onPreExecute()
	{
		// Disable blockView:
		if(blockView != null)
			blockView.setEnabled(false);

		if(duration > 0)
			animateView.startAnimation(getAnimationSet());
	}
	
	protected abstract AnimationSet getAnimationSet();

	@Override
	protected Void doInBackground(Void... params)
	{
		if(duration > 0)
		{
			try
			{
				Thread.sleep(duration);
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

		// Re-enable blockView:
		if(blockView != null)
			blockView.setEnabled(true);
	}

}
