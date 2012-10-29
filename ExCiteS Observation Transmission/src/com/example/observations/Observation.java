package com.example.observations;

public class Observation {

	private String decision;
	private Float lat;
	private Float lng;
	private Float alt;
	private Float gpsAcc;
	private Float bearing;
	private long timestamp;
	private Transaction transaction;

	public Observation(String decision, Float lat, Float lng,
			Float alt, Float gpsAcc, Float bearing, long timestamp) {
		super();
		this.decision = decision;
		this.lat = lat;
		this.lng = lng;
		this.alt = alt;
		this.gpsAcc = gpsAcc;
		this.bearing = bearing;
		this.timestamp = timestamp;
	}

	public String getDecision() {
		return decision;
	}

	public void setDecision(String decision) {
		this.decision = decision;
	}

	public Float getLat() {
		return lat;
	}

	public void setLat(Float lat) {
		this.lat = lat;
	}

	public Float getLng() {
		return lng;
	}

	public void setLng(Float lng) {
		this.lng = lng;
	}

	public Float getAlt() {
		return alt;
	}

	public void setAlt(Float alt) {
		this.alt = alt;
	}

	public Float getGpsAcc() {
		return gpsAcc;
	}

	public void setGpsAcc(Float gpsAcc) {
		this.gpsAcc = gpsAcc;
	}

	public Float getBearing() {
		return bearing;
	}

	public void setBearing(Float bearing) {
		this.bearing = bearing;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * @return the transaction
	 */
	public Transaction getTransaction() {
		return transaction;
	}

	/**
	 * @param transaction the transaction to set
	 */
	public void setTransaction(Transaction transaction) {
		this.transaction = transaction;
	}

}
