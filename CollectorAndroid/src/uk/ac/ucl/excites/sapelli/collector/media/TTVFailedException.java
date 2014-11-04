package uk.ac.ucl.excites.sapelli.collector.media;

/**
 * @author Ben
 *
 */
public class TTVFailedException extends Exception {

    private static final long serialVersionUID = 1L;
    
    private String text;
    
    public TTVFailedException(String text) {
    	super();
    	this.text = text;
    }
    
    public String getText() {
    	return text;
    }

}
