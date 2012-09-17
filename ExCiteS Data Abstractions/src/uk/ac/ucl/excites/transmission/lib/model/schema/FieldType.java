package uk.ac.ucl.excites.transmission.lib.model.schema;

/**
 * @author mstevens
 *
 */
public abstract class FieldType
{

	//STATIC
	public static final int TYPE_TAG_BOOLEAN = 0;
	public static final int TYPE_TAG_INTEGER = 1;
	public static final int TYPE_TAG_FLOAT = 2;
	public static final int TYPE_TAG_DOUBLE = 3;
	public static final int TYPE_TAG_UTF8_STRING = 4;
	
	//DYNAMIC
	private int tag;
	private Integer sizeBits;
	private boolean variableSize;
	private int minSizeBits;
	private int sizeStepBits;
	
	public FieldType(int tag, int sizeBits)
	{
		this.tag = tag;
		this.sizeBits = sizeBits;
		variableSize = false;		
	}
	
	public FieldType(int tag, int minSizeBits, int sizeStepBits)
	{
		this.tag = tag;
		this.minSizeBits = minSizeBits;
		this.sizeStepBits = sizeStepBits;
		variableSize = true;
	}

	public boolean isValidSize(int bits)
	{
		if(variableSize)
			return (bits > 0) && (bits % sizeStepBits == 0); 
		else
			return bits == sizeBits;
	}
	
	/**
	 * @return the tag
	 */
	public int getTag()
	{
		return tag;
	}

	/**
	 * @return the sizeBits
	 */
	public Integer getSizeBits()
	{
		return sizeBits;
	}

	/**
	 * @return the variableSize
	 */
	public boolean isVariableSize()
	{
		return variableSize;
	}

	/**
	 * @return the minSizeBits
	 */
	public int getMinSizeBits()
	{
		return minSizeBits;
	}

	/**
	 * @return the sizeStepBits
	 */
	public int getSizeStepBits()
	{
		return sizeStepBits;
	}

}
