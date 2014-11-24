package uk.ac.ucl.excites.sapelli.collector.load;

public class TTVUnsupportedLanguageException extends Exception {
	
    private static final long serialVersionUID = 1L;
    
	private String languageCode;
	
	public TTVUnsupportedLanguageException(String languageCode) {
		this.languageCode = languageCode;
	}

	public String getLanguageCode() {
		return languageCode;
	}
}
