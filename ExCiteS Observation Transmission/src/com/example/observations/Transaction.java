package com.example.observations;

import java.util.ArrayList;

public class Transaction {

	private static final int DEFAULT_SIZE = 10;
	
	private ArrayList<Observation> observations;
	private Integer ID;
	private boolean sent = false;
	private boolean received = false;

	public Transaction(Integer ID, boolean sent,
			boolean received) {
		super();
		this.observations = new ArrayList<Observation>();
		this.ID = ID;
		this.sent = sent;
		this.received = received;
	}

	public ArrayList<Observation> getObservations() {
		return observations;
	}

	public void addObservation(Observation observation)
	{
		if(observations.size() == DEFAULT_SIZE)
			throw new IllegalStateException("Transaction is full");
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

	public void send() {
		this.sent = true;
	}

	public void receive() {
		this.received = true;
	}

}
