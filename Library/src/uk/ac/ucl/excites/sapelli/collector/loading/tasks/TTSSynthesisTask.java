/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.loading.tasks;

/**
 * @author mstevens
 *
 */
public class TTSSynthesisTask implements LoadingTask
{
	
	//TODO private String language;
	private String textToSynthesise;
	private String audioFileRelativePath;

	/**
	 * @param textToSynthesise
	 * @param audioFileRelativePath
	 */
	private TTSSynthesisTask(String textToSynthesise, String audioFileRelativePath)
	{
		this.textToSynthesise = textToSynthesise;
		this.audioFileRelativePath = audioFileRelativePath;
	}

	/**
	 * @return the textToSynthesise
	 */
	public String getTextToSynthesise()
	{
		return textToSynthesise;
	}

	/**
	 * @return the audioFileRelativePath
	 */
	public String getAudioFileRelativePath()
	{
		return audioFileRelativePath;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.loading.tasks.LoadingTask#execute(uk.ac.ucl.excites.sapelli.collector.loading.tasks.LoadingTaskExecutor)
	 */
	@Override
	public void execute(LoadingTaskExecutor executor)
	{
		executor.execute(this);
	}

}
