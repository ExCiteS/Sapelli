package uk.ac.ucl.excites.observations.transmission;

public class SMSmessage {

	private String contents;
	private boolean sent;
	private Receiver receiver;

	public SMSmessage(String contents, Receiver receiver) {
		super();
		if (contents.length() > 160) {
			throw new IllegalStateException("SMS messages must not exceed the length of 160 characters");
		}
		this.contents = contents;
		this.receiver = receiver;

	}

	public void send() {

		// do something (see Michalis' code)
		sent = true;
	}

}
