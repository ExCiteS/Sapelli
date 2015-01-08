package uk.ac.ucl.excites.sapelli.transmission.model;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;

public class Receiver // TODO rename
{
	static public final int RECEIVER_ID_SIZE = 24; // bits
	static public final IntegerRangeMapping RECEIVER_ID_FIELD = IntegerRangeMapping.ForSize(0, RECEIVER_ID_SIZE); // unsigned(!) 24 bit integer
	
	static public final int RETRANSMIT_INTERVAL_SIZE_BITS = 32;
	private int projectId;
	private int correspondentName;
	private int retransmitIntervalSec;
	private boolean encrypt;
	
	public Receiver(int projectId, int correspondentName, int retransmitIntervalSec, boolean encrypt)
	{
		this.projectId = projectId;
		this.correspondentName = correspondentName;
		this.retransmitIntervalSec = retransmitIntervalSec;
		this.encrypt = encrypt;
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
	public int getCorrespondentName()
	{
		return correspondentName;
	}

	/**
	 * @param correspondentName the correspondentName to set
	 */
	public void setCorrespondentName(int correspondentName)
	{
		this.correspondentName = correspondentName;
	}

	/**
	 * @return the retransmitIntervalSec
	 */
	public int getRetransmitIntervalSec()
	{
		return retransmitIntervalSec;
	}

	/**
	 * @param retransmitIntervalSec the retransmitIntervalSec to set
	 */
	public void setRetransmitIntervalSec(int retransmitIntervalSec)
	{
		this.retransmitIntervalSec = retransmitIntervalSec;
	}

	/**
	 * @return the encrypt
	 */
	public boolean isEncrypt()
	{
		return encrypt;
	}

	/**
	 * @param encrypt the encrypt to set
	 */
	public void setEncrypt(boolean encrypt)
	{
		this.encrypt = encrypt;
	}
	
}
