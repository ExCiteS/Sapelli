/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.compression;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;

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