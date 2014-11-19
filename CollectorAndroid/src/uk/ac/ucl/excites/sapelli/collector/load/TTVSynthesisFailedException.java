package uk.ac.ucl.excites.sapelli.collector.load;

/**
 * @author Ben
 *
 */
public class TTVSynthesisFailedException extends Exception {

    private static final long serialVersionUID = 1L;
    
    private String text;
    
    public TTVSynthesisFailedException(String text) {
    	super();
    	this.text = text;
    }
    
    public String getText() {
    	return text;
    }

}
