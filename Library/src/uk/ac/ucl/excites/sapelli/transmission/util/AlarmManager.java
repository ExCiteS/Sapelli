package uk.ac.ucl.excites.sapelli.transmission.util;

public interface AlarmManager
{
	/**
	 * Set a repeating alarm that starts after {@code delayMillis} milliseconds, executes {@code action} immediately and then continues to do so every {@code intervalMillis}
	 * milliseconds until cancelled. 
	 * @param delayMillis the initial delay before the action should execute for the first time
	 * @param intervalMillis the interval between executions of the action
	 * @param action the action to perform on alarm timeout
	 */
	public void setAlarm(int delayMillis, int intervalMillis, Runnable action);
	
	/**
	 * Set a non-repeating alarm to execute {@code action} once after {@code delayMillis} milliseconds.
	 * @param delayMillis the delay before the action should execute
	 * @param action the action to execute when the alarm expires
	 */
	public void setAlarm(int delayMillis, Runnable action);
	
	/**
	 *  Cancels the alarm associated with {@code action} if one is found.
	 *  @param action the action (object) that was associated with the alarm when it was set
	 *  @return whether or not an alarm was found to be associated with {@code action}.
	 */
	public boolean cancelAlarm(Runnable action);
}
