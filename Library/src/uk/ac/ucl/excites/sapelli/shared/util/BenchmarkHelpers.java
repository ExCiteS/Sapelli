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

package uk.ac.ucl.excites.sapelli.shared.util;

/**
 * @author mstevens
 *
 */
public final class BenchmarkHelpers
{

	/**
	 * Should never be instantiated.
	 */
	private BenchmarkHelpers() {}
	
	/**
	 * @author mstevens
	 */
	static public interface Test
	{
		public abstract void run() throws Exception;
	}
	
	/**
	 * @param runnable
	 * @param runs
	 * @return the average execution time in ns
	 * @throws Exception 
	 */
	static final public long bench(final int runs, final Test test) throws Exception
	{
		long sumNS = 0;
		for(int i = 0; i < runs; i++)
		{
			long startTimeNS = System.nanoTime();
			test.run();
			sumNS += System.nanoTime() - startTimeNS;
		}
		return sumNS / (long) runs;
	}

}
