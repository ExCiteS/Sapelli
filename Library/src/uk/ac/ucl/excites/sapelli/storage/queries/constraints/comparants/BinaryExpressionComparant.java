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
public class BinaryExpressionComparant extends NumberComparant
{	
	
	static public enum BinaryOperator
	{
		//concatenate,
		multiply,
		divide,
		modulo,
		add,
		subtract,
		bitShiftLeft,
		bitShiftRight,
		bitAnd,
		bitOr,
		// others later?
		
	}
	
	private final Comparant<? extends Number> lhs;
	private final BinaryOperator operator;
	private final Comparant<? extends Number> rhs;
	
	/**
	 * @param lhs
	 * @param operator
	 * @param rhs
	 */
	public BinaryExpressionComparant(Comparant<? extends Number> lhs, BinaryOperator operator, Comparant<? extends Number> rhs)
	{
		this.lhs = lhs;
		this.operator = operator;
		this.rhs = rhs;
	}

	@Override
	public Number getValue(Record record)
	{
		Number lhsValue = lhs.getValue(record);
		Number rhsValue = rhs.getValue(record);
		switch(operator)
		{
			case multiply:
				return Numbers.mul(lhsValue, rhsValue);
			case divide:
				return Numbers.div(lhsValue, rhsValue);
			case modulo:
				return Numbers.remainder(lhsValue, rhsValue);
			case add:
				return Numbers.add(lhsValue, rhsValue);
			case subtract:
				return Numbers.sub(lhsValue, rhsValue);
			case bitShiftLeft:
				return Numbers.shiftLeft(lhsValue, rhsValue);
			case bitShiftRight:
				return Numbers.shiftRight(lhsValue, rhsValue);
			case bitAnd:
				return Numbers.and(lhsValue, rhsValue);
			case bitOr:
				return Numbers.or(lhsValue, rhsValue);
			default:
				throw new UnsupportedOperationException("Unsupported " + BinaryOperator.class.getSimpleName() + " \"" + operator.name() + "\n!");
		}
	}
	
}
