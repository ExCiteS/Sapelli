/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.util.Comparator;

/**
 * Column with support for comparing values of generic type {@code T}.
 * 
 * @param <T>
 * @author mstevens
 */
public abstract class ComparatorColumn<T> extends Column<T> implements Comparator<T>
{

	public ComparatorColumn(Class<T> type, String name, boolean optional)
	{
		super(type, name, optional);
	}
	
	public int retrieveAndCompareValues(Record record1, Record record2)
	{
		return compare(retrieveValue(record1), retrieveValue(record2));
	}
	
	public int retrieveAndCompareToValue(Record record, T value)
	{
		return compare(retrieveValue(record), value);
	}
	
	@SuppressWarnings("unchecked")
	public int retrieveAndCompareToObject(Record record, Object value)
	{
		return compare(retrieveValue(record), (T) value);
	}
	
	/**
	 * @param lhs left-hand side value, possibly null
	 * @param rhs right-hand side value, possibly null 
	 * @return comparison result
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 * @see <a href="http://stackoverflow.com/a/128220/1084488">http://stackoverflow.com/a/128220/1084488</a>
	 */
	@Override
	public int compare(T lhs, T rhs)
	{
		return lhs == null ?
				(rhs == null ? 0 : Integer.MIN_VALUE) :
				(rhs == null ? Integer.MAX_VALUE : compareNonNull(lhs, rhs));
	}
	
	/**
	 * To be implemented by subclasses, arguments are guaranteed to both be non-null.
	 * 
	 * @param lhs left-hand side value, guaranteed non-null
	 * @param rhs right-hand side value, guaranteed non-null
	 * @return comparison result
	 */
	protected abstract int compareNonNull(T lhs, T rhs);

}
