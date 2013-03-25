package uk.ac.uk.excites.server;

public class SMS
{
	private String id;
	private String phoneNumber;
	private String timestamp;
	private String data;

	public SMS(String id, String phoneNumber, String timestamp, String data)
	{
		this.id = id;
		this.phoneNumber = phoneNumber;
		this.timestamp = timestamp;
		this.data = data;
	}
}