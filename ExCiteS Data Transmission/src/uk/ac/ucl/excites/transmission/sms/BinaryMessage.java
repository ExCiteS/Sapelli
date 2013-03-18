/**
 * 
 */
package uk.ac.ucl.excites.transmission.sms;

/**
 * @author mstevens
 *
 */
public class BinaryMessage extends Message
{
	
	public BinaryMessage(SMSSender sender, SMSReceiver receiver, SMSTransmission transmission)
	{
		super(sender, receiver, transmission);
	}

	//Static
	public static final int MAX_TOTAL_SIZE = 133; //in Bytes (Android takes 7 bits for the header)
	public static final int HEADER_SIZE = 2; //TODO determine actual needed header size
	public static final int MAX_CONTENT_SIZE = MAX_TOTAL_SIZE - HEADER_SIZE;
	
	//Dynamic
	public byte[] content;
	
	@Override
	public int getMaxContentSize()
	{
		return MAX_CONTENT_SIZE;
	}

	@Override
	public void setContent(byte[] content)
	{
		if(content.length >= getMaxContentSize())
			throw new IllegalArgumentException("Content is too long (max size: " + getMaxContentSize() + ")");
		this.content = content;
	}

	@Override
	public void send()
	{
		if(content == null)
			throw new IllegalStateException("Cannot send message, no content has been set."); //TODO perhaps in the future we could allow empty messages to be sent (what would this mean?)
		//BitArray dataToSend = new BitArray((HEADER_SIZE + content.length) * 8);
		//Construct header:
		
		
		//Copy contents:
		//dataToSend.setBytes(HEADER_SIZE * 8, content);
		//Get byte[]
		//byte[] bytes = dataToSend.toBytes();
		
		
		
		sender.send(this);
	}

}
