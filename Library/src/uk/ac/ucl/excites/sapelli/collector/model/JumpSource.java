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

package uk.ac.ucl.excites.sapelli.collector.model;


/**
 * Super class for Field & Trigger
 * 
 * @author mstevens
 */
public abstract class JumpSource
{

	protected Field jump;
	protected FieldParameters nextFieldArgs;
	
	public void setJump(Field target)
	{
		if(this == target)
			throw new IllegalArgumentException("Jumping to one's self is not allowed!");
		if(jump != null)
			throw new IllegalStateException("Cannot change jump target once it has been set!");
		this.jump = target;
	}
	
	public Field getJump()
	{
		return jump;
	}
	
	public void setNextFieldArguments(FieldParameters argumentsForNextField)
	{
		if(nextFieldArgs != null)
			throw new IllegalStateException("Cannot change arguments once they have been set!");
		this.nextFieldArgs = argumentsForNextField;
	}

	public boolean hasNextFieldArguements()
	{
		return nextFieldArgs != null && nextFieldArgs != FieldParameters.EMPTY;
	}
	
	public FieldParameters getNextFieldArguments()
	{
		return nextFieldArgs != null ? nextFieldArgs : FieldParameters.EMPTY;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof JumpSource)
		{
			JumpSource that = (JumpSource) obj;
			return	(this.jump != null ? that.jump != null && this.jump.id.equals(that.jump.id) : that.jump == null) && // DO NOT INCLUDE jump ITSELF HERE (otherwise we may create an endless loop!)
					(this.nextFieldArgs != null ? this.nextFieldArgs.equals(that.nextFieldArgs) : that.nextFieldArgs == null);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (jump == null ? 0 : jump.id.hashCode()); // DO NOT INCLUDE jump ITSELF HERE (otherwise we may create an endless loop!)
		hash = 31 * hash + (nextFieldArgs == null ? 0 : nextFieldArgs.hashCode());
		return hash;
	}
	
}
