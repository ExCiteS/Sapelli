package uk.ac.ucl.excites.sapelli.collector.media;

/**
 * @author Ben
 *
 */
public class TTSFailedException extends Exception {

    private static final long serialVersionUID = 1L;
    
    private String text;
    
    public TTSFailedException(String text) {
    	super();
    	this.text = text;
    }
    
    public String getText() {
    	return text;
    }

}
