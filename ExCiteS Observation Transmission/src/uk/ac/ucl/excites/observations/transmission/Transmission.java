package uk.ac.ucl.excites.observations.transmission;

import java.util.ArrayList;

public class Transmission {

	private static final int DEFAULT_SIZE = 100;
	
	private ArrayList<Observation> observations;
	
	private Integer ID;
	private boolean sent = false;
	private boolean received = false;
	private Receiver receiver;

	public Transmission(Integer ID, Receiver receiver)
	{
		super();
		this.observations = new ArrayList<Observation>();
		this.ID = ID;
		this.sent = false;
		this.received = false;
		this.receiver = receiver;
	}

	public ArrayList<Observation> getObservations() {
		return observations;
	}

	public void addObservation(Observation observation)
	{
		if(observations.size() == DEFAULT_SIZE)
			throw new IllegalStateException("Transmission is full");
		observations.add(observation);
		observation.setTransaction(this);
	}
	
	public boolean isEmpty()
	{
		return observations.size() == 0;
	}

	public boolean isFull()
	{
		return observations.size() == DEFAULT_SIZE;
	}
	
	public Integer getID() {
		return ID;
	}

	public void setID(Integer iD) {
		ID = iD;
	}

	public boolean isSent() {
		return sent;
	}

	public void setSent(boolean sent) {
		this.sent = sent;
	}

	public boolean isReceived() {
		return received;
	}

	public void setReceived(boolean received) {
		this.received = received;
	}

	public void send()
	{
		/* create and send sms messages */
		this.sent = true;
	}
	
	public void resend(int part)
	{
		
	}

	public void receive() {
		this.received = true;
	}

}
