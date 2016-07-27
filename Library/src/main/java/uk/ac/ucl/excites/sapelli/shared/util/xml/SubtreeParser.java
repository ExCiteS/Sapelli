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

package uk.ac.ucl.excites.sapelli.shared.util.xml;

/**
 * @author mstevens
 *
 */
public abstract class SubtreeParser<O extends Handler> extends Handler
{

	protected final O owner;
	private final String rootElementQName;
	
	public SubtreeParser(O owner, String rootElementQName)
	{
		this.owner = owner;
		this.rootElementQName = rootElementQName;
	}
	
	@Override
	public void startDocument()
	{
		throw new UnsupportedOperationException("XMLSubtreeParsers are not meant to parse entire documents");
	}
	
	@Override
	public void endDocument()
	{
		throw new UnsupportedOperationException("XMLSubtreeParsers are not meant to parse entire documents");
	}
	
	protected void activate()
	{
		owner.activateSubtreeParser(this);
	}
	
	public void deactivate()
	{
		owner.deactivateSubtreeParser(this);
		reset();
	}
	
	public boolean isActive()
	{
		return owner.getActiveSubtreeParser() == this;
	}
	
	public final String getRootElementQName()
	{
		return rootElementQName;
	}
	
	protected abstract void reset();
	
	protected abstract boolean isSingleUse();
	
}
