/**
 * 
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
			return	(this.jump != null ? that.jump != null && this.jump.getID().equals(that.jump.getID()) : that.jump == null) && // DO NOT INCLUDE jump ITSELF HERE (otherwise we may create an endless loop!)
					(this.nextFieldArgs != null ? this.nextFieldArgs.equals(that.nextFieldArgs) : that.nextFieldArgs == null);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (jump == null ? 0 : jump.getID().hashCode()); // DO NOT INCLUDE jump ITSELF HERE (otherwise we may create an endless loop!)
		hash = 31 * hash + (nextFieldArgs == null ? 0 : nextFieldArgs.hashCode());
		return hash;
	}
	
}
