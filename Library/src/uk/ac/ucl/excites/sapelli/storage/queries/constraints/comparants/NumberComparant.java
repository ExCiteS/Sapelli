/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants;

import com.stefanmuenchow.arithmetic.Numbers;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class NumberComparant extends ComparingComparant<Number, Number>
{

	public int compare(Record record, Comparant<? extends Number> another)
	{
		return Numbers.compare(this.getValue(record), another.getValue(record));
	}

}
