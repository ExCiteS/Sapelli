package uk.ac.ucl.excites.sapelli.transmission.model;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;

public class Sender // TODO rename
{
	static public final int SENDER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping SENDER_ID_FIELD = IntegerRangeMapping.ForSize(0, SENDER_ID_SIZE); // unsigned(!) 24 bit integer
	
	private int projectId;
	private String correspondentName;
	private boolean ack;
	
	public Sender(int projectId, String correspondentName, boolean ack)
	{
		super();
		this.projectId = projectId;
		this.correspondentName = correspondentName;
		this.ack = ack;
	}

	/**
	 * @return the projectId
	 */
	public int getProjectId()
	{
		return projectId;
	}

	/**
	 * @param projectId the projectId to set
	 */
	public void setProjectId(int projectId)
	{
		this.projectId = projectId;
	}

	/**
	 * @return the correspondentName
	 */
	public String getCorrespondentName()
	{
		return correspondentName;
	}

	/**
	 * @param correspondentName the correspondentName to set
	 */
	public void setCorrespondentName(String correspondentName)
	{
		this.correspondentName = correspondentName;
	}

	/**
	 * @return the ack
	 */
	public boolean isAck()
	{
		return ack;
	}

	/**
	 * @param ack the ack to set
	 */
	public void setAck(boolean ack)
	{
		this.ack = ack;
	}

}
