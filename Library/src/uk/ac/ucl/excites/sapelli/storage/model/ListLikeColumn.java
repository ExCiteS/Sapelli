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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * Implemented by {@link ListColumn} and {@link StringColumn}.
 * Provides alternative {@link Column#parse(String)} and {@link Column#toString(Object)} methods
 * which allow to disable the expectation and use of serialisation delimiting.
 * 
 * @author mstevens
 */
public interface ListLikeColumn<T>
{

	/**
	 * @param valueString the {@link String} to parse, should not be {@code null} (as that would represent a {@code null} list), an empty {@code String} is allowed if {@code undelimited} is {@code true} (the given empty String then represents a non-null but empty List/String)
	 * @param undelimited if {@code true} the given valueString is excepted to *not* have serialisation delimiters (and is thus allowed to be empty), if {code false} the given valueString is excepted to have serialisation delimiters (and thus cannot be empty)  
	 * @return the parsed value as type {@code <T>}
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	public T parse(String valueString, boolean undelimited) throws ParseException, IllegalArgumentException, NullPointerException;
	
	/**
	 * @param value assumed to be non-null! But an empty list object is allowed.
	 * @param undelimited if {@code true} the returned String will *not* have serialisation delimiters (and thus may be empty if the given value was an empty List/String), if {@code false} the returned String will have serialisation delimiters (and thus will never be empty, even if the given value was an empty List/String)
	 * @return
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	public String toString(T value, boolean undelimited);

}
