package uk.ac.ucl.excites.sapelli.collector.project.model;


/**
 * @author mstevens
 *
 */
public class ValidationResult
{
	
	static public ValidationResult Success(Field field)
	{
		return new ValidationResult(field, true, null);
	}
	
	static public ValidationResult Failure(Field field, String reason)
	{
		return new ValidationResult(field, false, reason);
	}

	private Field field;
	private boolean successful;
	private String failureReason;
	
	/**
	 * @param field
	 * @param value
	 * @param successfull
	 */
	private ValidationResult(Field field, boolean successful, String failureReason)
	{
		this.field = field;
		this.successful = successful;
		this.failureReason = failureReason;
	}
	
	/**
	 * @return the field
	 */
	public Field getField()
	{
		return field;
	}

	/**
	 * @return the successful
	 */
	public boolean isSuccessful()
	{
		return successful;
	}
	
	/**
	 * @return the failureReason
	 */
	public String getFailureReason()
	{
		return failureReason;
	}
	
}
