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

package uk.ac.ucl.excites.sapelli.transmission.compression;

import java.io.IOException;
import java.util.concurrent.Callable;

/**
 * @author mstevens
 *
 */
public abstract class Compressor
{

	public abstract CompressorFactory.Compression getMode();
	
	public abstract byte[] compress(byte[] data) throws IOException;

	public abstract byte[] decompress(byte[] compressedData) throws IOException;
	
//	public class CompressorCallable implements Callable<CompressorResult>
//	{
//
//		private byte[] uncompressedData;
//		
//		public CompressorCallable(byte[] uncompressedData)
//		{
//			this.uncompressedData = uncompressedData;
//		}
//		
//		@Override
//		public CompressorResult call() throws Exception
//		{
//			return new CompressorResult(getMode(), com, ratio)
//		}
//		
//	}

}
