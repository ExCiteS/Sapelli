/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model;


/**
 * @author mstevens, Michalis Vitos
 * 
 */
public interface JumpSource
{

	public void setJump(Field target);
	
	public Field getJump();
	
	public void setNextFieldArguments(FieldParameters argumentsForNextField);
	
	public FieldParameters getNextFieldArguments();
	
}
