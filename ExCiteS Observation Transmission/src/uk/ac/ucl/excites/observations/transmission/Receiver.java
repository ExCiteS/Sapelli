package uk.ac.ucl.excites.observations.transmission;

public class Receiver {

	private String phoneNumber;
	private boolean senderIntroduced;
	private long timeReceived;
	private long timeSent;

	/**
	 * @return the phoneNumber
	 */
	public String getPhoneNumber() {
		return phoneNumber;
	}

	/**
	 * @return the senderIntroduced
	 */
	public boolean isSenderIntroduced() {
		return senderIntroduced;
	}

	/**
	 * @return the timeReceived
	 */
	public long getTimeReceived() {
		return timeReceived;
	}

	/**
	 * @return the timeSent
	 */
	public long getTimeSent() {
		return timeSent;
	}

}
