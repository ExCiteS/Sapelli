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

package uk.ac.ucl.excites.sapelli.collector.media;

import java.util.Collections;
import java.util.List;

/**
 * Abstract notion of an object that plays audio feedback items and animates their corresponding UI elements at the same time.
 *
 * @param <V>
 * 
 * @author mstevens, benelliott, Michalis Vitos
 */
public abstract class AudioFeedbackController<V>
{
	
	public static final int ANIMATION_SHAKE = 0;
	public static final int ANIMATION_ALPHA = 1;
	
	/**
	 * Play a single audio feedback job.
	 * @param job - the audio feedback job to play
	 */
	public void play(PlaybackJob job)
	{
		play(Collections.<PlaybackJob> singletonList(job));
	}
	
	/**
	 * Play a sequence of audio feedback jobs.
	 * @param sequence - the list of audio feedback jobs to play.
	 */
	public abstract void play(List<PlaybackJob> sequence);
	
	/**
	 * Stop playing the current job.
	 */
	public abstract void stop();
	
	/**
	 * Destroy all resources required for audio feedback.
	 */
	public abstract void destroy();

	/**
	 * @param soundRelativePath
	 * @return a PlaybackJob
	 */
	public PlaybackJob newPlaybackJob(String soundRelativePath)
	{
		return new PlaybackJob(soundRelativePath);
	}
	
	/**
	 * @param soundRelativePath
	 * @param viewToAnimate
	 * @param animation
	 * @return a PlaybackJob
	 */
	public PlaybackJob newPlaybackJob(String soundRelativePath, V viewToAnimate, int animation)
	{
		return new PlaybackJob(soundRelativePath, viewToAnimate, animation);
	}
	
	/**
	 * A class that encapsulates the concept of an "audio feedback job" with a filepath for the sound to play
	 * and a reference to the UI object (e.g. View) to animate while doing so.
	 * 
	 * @author mstevens
	 */
	public class PlaybackJob
	{
		
		/*package*/ final String soundRelativePath;
		/*package*/ final V viewToAnimate;
		/*package*/ final int animation;
		
		/**
		 * Creates a PlaybackJob with the provided sound filepath but no UI object to animate.
		 * 
		 * @param soundRelativePath - relative filepath of the sound to play
		 */
		private PlaybackJob(String soundRelativePath)
		{
			this(soundRelativePath, null, -1);
		}
		
		/**
		 * Creates a PlaybackJob with the provided sound filepath and UI object to animate.
		 * 
		 * @param soundRelativePath - relative filepath of the sound to play
		 * @param viewToAnimate - the UI object to animate while playing the sound (may be null)
		 * @param animation - indicates the animation to use
		 */
		private PlaybackJob(String soundRelativePath, V viewToAnimate, int animation)
		{
			this.soundRelativePath = soundRelativePath;
			this.viewToAnimate = viewToAnimate;
			this.animation = animation;
		}
		
	}

}
