/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;

/**
 * Dummy compressor (leaves data unchanged)
 * 
 * @author mstevens
 *
 */
public class DummyCompressor extends Compressor
{

	@Override
	public byte[] compress(byte[] data) throws IOException
	{
		return data;
	}

	@Override
	public byte[] decompress(byte[] compressedData) throws IOException
	{
		return compressedData;
	}

	@Override
	public Compression getMode()
	{
		return Compression.NONE;
	}
	
}
