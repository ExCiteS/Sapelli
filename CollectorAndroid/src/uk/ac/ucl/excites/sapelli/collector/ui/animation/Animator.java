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

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import android.os.AsyncTask;
import android.view.View;
import android.view.animation.AnimationSet;

/**
 * AsyncTask Class that blocks the UI thread for {@link Animator#duration} milliseconds and executes the animation to the {@link Animator#view}. After the end of
 * the duration and the animation is over, the {@link Animator#runnableTask} will be run (unless it is {@code null}).
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public abstract class Animator extends AsyncTask<Void, Void, Void>
{
	
	protected long duration;
	private Runnable taskAfterAnimation; // may be null
	private View animateView;
	private View blockView; // may be null
	private boolean blockViewEnabled;
	private Controller controller; // may be null

	/**
	 * @param duration  running time of the animation (in ms)
	 * @param taskAfterAnimation  Runnable that is to be executed after the animation is finished (may be null)
	 * @param animateView  View to which the animation will be applied
	 * @param blockView  View which is to be disabled while the animation is running (may be null)
	 */
	public Animator(long duration, Runnable taskAfterAnimation, View animateView, View blockView)
	{
		this.duration = duration;
		this.taskAfterAnimation = taskAfterAnimation;
		this.animateView = animateView;
		this.blockView = blockView;
	}
	
	/**
	 * @param duration  running time of the animation (in ms)
	 * @param taskAfterAnimation  Runnable that is to be executed after the animation is finished (may be null)
	 * @param animateView  View to which the animation will be applied
	 * @param controller  if a non-null Controller instance is passed the Controller's blockUI() method will be used to block all UI interactions during animation
	 */
	public Animator(long duration, Runnable taskAfterAnimation, View animateView, Controller controller)
	{
		this.duration = duration;
		this.taskAfterAnimation = taskAfterAnimation;
		this.animateView = animateView;
		this.controller = controller;
	}

	@Override
	protected void onPreExecute()
	{
		if(duration > 0)
		{
			// Disable blockView:
			if(blockView != null)
			{
				blockViewEnabled = blockView.isEnabled();
				blockView.setEnabled(false);
			}
			// Block the whole UI:
			if(controller != null)
				controller.blockUI();
			// Animate...
			animateView.startAnimation(getAnimationSet());
		}
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
		if(taskAfterAnimation != null)
			taskAfterAnimation.run();
		
		// Re-enable blockView:
		if(blockView != null)
			blockView.setEnabled(blockViewEnabled);
		// Re-enable the UI
		if(controller != null)
			controller.unblockUI();
	}

}
