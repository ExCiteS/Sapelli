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

package uk.ac.ucl.excites.sapelli.shared.util.xml;

import org.xml.sax.Attributes;

import uk.ac.ucl.excites.sapelli.shared.util.Parameters;

/**
 * @author mstevens
 *
 */
public class XMLAttributes extends Parameters
{

	private Attributes attributes;
	
	/**
	 * @param attributes
	 */
	public XMLAttributes(Attributes attributes)
	{
		this.attributes = attributes;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.util.Parameters#getValue(java.lang.String)
	 */
	@Override
	public String getValue(String param)
	{
		return attributes.getValue(param);
	}

	@Override
	public boolean contains(String param)
	{
		return attributes.getIndex(param) != -1;
	}

	/**
	 * @return the attributes
	 */
	public Attributes getAttributes()
	{
		return attributes;
	}
	
}
