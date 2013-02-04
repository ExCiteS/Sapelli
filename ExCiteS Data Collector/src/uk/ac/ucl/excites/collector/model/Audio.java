/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

/**
 * @author Michalis Vitos
 * 
 */
public class Audio extends MediaAttachment
{

	private String pathRecordingImage;
	private String pathStopImage;

	public Audio()
	{
		// Set the min to 0 and the max to the maximum value of an Integer
		this(0, Integer.MAX_VALUE, null);
	}

	/**
	 * @param min
	 * @param max
	 * @param disableChoice
	 */
	public Audio(int min, int max, Choice disableChoice)
	{
		this.min = min;
		this.max = max;
		this.disableChoice = disableChoice;
	}

	/**
	 * @return the pathRecordingImage
	 */
	public String getPathRecordingImage()
	{
		return pathRecordingImage;
	}

	/**
	 * @param pathRecordingImage
	 *            the pathRecordingImage to set
	 */
	public void setPathRecordingImage(String pathRecordingImage)
	{
		this.pathRecordingImage = pathRecordingImage;
	}

	/**
	 * @return the pathStopImage
	 */
	public String getPathStopImage()
	{
		return pathStopImage;
	}

	/**
	 * @param pathStopImage
	 *            the pathStopImage to set
	 */
	public void setPathStopImage(String pathStopImage)
	{
		this.pathStopImage = pathStopImage;
	}

}
