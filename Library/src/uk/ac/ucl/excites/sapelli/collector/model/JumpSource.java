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
		this.jump = target;
	}
	
	public Field getJump()
	{
		return jump;
	}
	
	public void setNextFieldArguments(FieldParameters argumentsForNextField)
	{
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
	
}
