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

import java.math.BigInteger;
import java.util.Arrays;

/**
 * @author mstevens
 *
 */
public final class BigIntegerUtils
{

	static public final BigInteger ZERO = BigInteger.ZERO;
	static public final BigInteger ONE = BigInteger.ONE;
	static public final BigInteger TWO = BigInteger.valueOf(2);
	
	/**
	 * @param numberOfBits
	 * @param signed
	 * @return
	 */
	static public BigInteger GetMinValue(int numberOfBits, boolean signed)
	{
		if(numberOfBits < 0)
			throw new IllegalArgumentException("Number of bits cannot be negative!");
		return !signed || numberOfBits == 0 ?	ZERO :
												TWO.pow(numberOfBits - 1).negate();
		/*// version with longs (for reference):
		return !signed || numberOfBits == 0 ?	0l :
												(long) (- Math.pow(2, numberOfBits - 1));*/
	}
	
	/**
	 * @param numberOfBits
	 * @param signed
	 * @return
	 */
	static public BigInteger GetMaxValue(int numberOfBits, boolean signed)
	{
		if(numberOfBits < 0)
			throw new IllegalArgumentException("Number of bits cannot be negative!");
		return numberOfBits == 0 ?	BigInteger.ZERO :
									(signed ? 	TWO.pow(numberOfBits - 1).subtract(ONE) :
												TWO.pow(numberOfBits).subtract(ONE));
		/*// version with longs (for reference):
		return numberOfBits == 0 ?	0l :
									(signed ?	(long) (Math.pow(2, size - 1) - 1) : 
												(long) (Math.pow(2, size) - 1));*/
	}
	
	/**
	 * @param numberOfBits
	 * @return
	 */
	static public BigInteger NumberOfPossibleValues(int numberOfBits)
	{
		if(numberOfBits < 0)
			throw new IllegalArgumentException("Number of bits cannot be negative!");
		return TWO.pow(numberOfBits);
	}
	
	/**
	 * Computes a hash code for the given BigInteger.
	 * 
	 * This method provides an platform-independent and saver alternative to
	 * {@link BigInteger#hashCode()}.
	 * Android's implementation of that method is different (i.e. producing different
	 * hash codes for the same values!) compared to the Oracle JRE/JDK implementation.
	 * On top of that the is a known (and only very recently fixed) race condition in
	 * Android's implementation: https://code.google.com/p/android/issues/detail?id=193959
	 * 
	 * Note that this method conforms to neither the Oracle nor the Android implementation
	 * of {@link BigInteger#hashCode()}.
	 *
	 * @return hash code for this BigInteger.
	 */
	static public int hashCode(BigInteger bigInteger)
	{
		if(bigInteger == null)
			return 0;
		return Arrays.hashCode(bigInteger.toByteArray());
	}
	
	private BigIntegerUtils()
	{
		// should not be instantiated
	}
	
}
