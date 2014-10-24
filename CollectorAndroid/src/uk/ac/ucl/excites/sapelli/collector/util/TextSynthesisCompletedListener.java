package uk.ac.ucl.excites.sapelli.collector.util;

/**
 * Interface for classes interested in knowing when a text-to-file synthesis job has
 * been completed.
 * 
 * @author benelliott
 *
 */
public interface TextSynthesisCompletedListener {
	
	public void onTextSynthesisCompleted(String text, String filepath);

}
