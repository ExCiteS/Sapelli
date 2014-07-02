package uk.ac.ucl.excites.sapelli.transmission.compression;

import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;

public class CompressorResult
{
	
	private Compression mode;
	private byte[] compressedData;
	private float ratio;
	
	/**
	 * @param mode
	 * @param compressedData
	 * @param ratio
	 */
	public CompressorResult(Compression mode, byte[] compressedData, float ratio)
	{
		this.mode = mode;
		this.compressedData = compressedData;
		this.ratio = ratio;
	}

	/**
	 * @return the mode
	 */
	public Compression getMode()
	{
		return mode;
	}

	/**
	 * @return the compressedData
	 */
	public byte[] getCompressedData()
	{
		return compressedData;
	}

	/**
	 * @return the ratio
	 */
	public float getRatio()
	{
		return ratio;
	}
	
	@Override
	public String toString()
	{
		return mode + "-compressed data is " + compressedData.length + " bytes long (" + CompressorFactory.RATIO_FORMAT.format(ratio * 100.0f) + " %)"; 
	}
	
}