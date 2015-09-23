/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;

/**
 * @author mstevens
 *
 * @param <N>
 */
public abstract class NumberColumn<N extends Number> extends ComparableColumn<N>
{

	private static final long serialVersionUID = 2L;

	public NumberColumn(String name, boolean optional)
	{
		super(name, optional);
	}

}
