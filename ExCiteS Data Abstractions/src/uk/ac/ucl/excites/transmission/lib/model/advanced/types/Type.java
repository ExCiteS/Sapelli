package uk.ac.ucl.excites.transmission.lib.model.advanced.types;

import uk.ac.ucl.excites.transmission.lib.model.advanced.Field;

/**
 * @author mstevens
 *
 */
public abstract class Type
{

	private Integer sizeBits;
	private int minSizeBits;
	private int sizeStepBits;
	
	public Type(int tag, int sizeBits)
	{
		//this.tag = tag;
		this.sizeBits = sizeBits;	
	}
	
	public Type(int tag, int minSizeBits, int sizeStepBits)
	{
		//this.tag = tag;
		this.minSizeBits = minSizeBits;
		this.sizeStepBits = sizeStepBits;
	}

//	public boolean isValidSize(int bits)
	//{
//		if(variableSize)
//			return (bits > 0) && (bits % sizeStepBits == 0); 
//		else
//			return bits == sizeBits;
//	}
	
//	/**
//	 * @return the tag
//	 */
//	public int getTag()
//	{
//		return tag;
//	}

	/**
	 * @return the sizeBits
	 */
	public Integer getSizeBits()
	{
		return sizeBits;
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

	public abstract void encode(Field field, byte[] bytes, int startBit, int sizeBits);
	
	public abstract Field newField();
	 
	
}
