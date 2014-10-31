/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.media;

import java.util.Collections;
import java.util.List;

/**
 *
 * @param <V>
 * 
 * @author mstevens, Ben
 */
public abstract class AbstractAudioFeedbackController<V>
{
	
	/**
	 * @param job
	 */
	public void play(PlaybackJob job)
	{
		play(Collections.singletonList(job));
	}
	
	/**
	 * @param sequence
	 */
	public abstract void play(List<PlaybackJob> sequence);
	
	/**
	 * @author mstevens
	 */
	public class PlaybackJob
	{
		
		/*package*/ final String soundRelativePath;
		/*package*/ final V viewToAnimate;
		
		/**
		 * @param soundRelativePath
		 * @param viewToAnimate
		 */
		private PlaybackJob(String soundRelativePath, V viewToAnimate)
		{
			this.soundRelativePath = soundRelativePath;
			this.viewToAnimate = viewToAnimate;
		}
		
	}

}
