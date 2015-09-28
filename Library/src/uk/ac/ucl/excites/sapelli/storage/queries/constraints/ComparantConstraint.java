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

package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants.ColumnComparant;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants.Comparant;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants.ComparingComparant;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants.LiteralComparant;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Rule-based Constraint class
 * 
 * @author mstevens
 */
public class ComparantConstraint<X> extends Constraint
{
	
	// STATICS-------------------------------------------------------
	static public enum Comparison
	{
		SMALLER, 			/* < */
		SMALLER_OR_EQUAL,	/* <= */
		EQUAL,				/* = */
		NOT_EQUAL,			/* != */
		GREATER_OR_EQUAL,	/* >= */
		GREATER;			/* > */
		
		public Comparison negate()
		{
			switch(this)
			{
				case SMALLER : return Comparison.GREATER_OR_EQUAL;
				case SMALLER_OR_EQUAL : return Comparison.GREATER;
				case EQUAL : return Comparison.NOT_EQUAL;
				case NOT_EQUAL : return Comparison.EQUAL;
				case GREATER_OR_EQUAL : return Comparison.SMALLER;
				case GREATER : return Comparison.SMALLER_OR_EQUAL;
			}
			return null; // should never happen
		}
	}
	
	static public final String[] COMPARISON_STRINGS = new String[]
	{	
		"smaller",
		"smallerEqual",
		"equal",
		"different", "notEqual",
		"greaterEqual",
		"greater"
	};
	
	static public Comparison parseComparisonString(String comparisonString) throws ParseException
	{
		if(COMPARISON_STRINGS[0].equalsIgnoreCase(comparisonString))
			return Comparison.SMALLER;
		if(COMPARISON_STRINGS[1].equalsIgnoreCase(comparisonString))
			return Comparison.SMALLER_OR_EQUAL;
		if(COMPARISON_STRINGS[2].equalsIgnoreCase(comparisonString))
			return Comparison.EQUAL;
		if(COMPARISON_STRINGS[3].equalsIgnoreCase(comparisonString) || COMPARISON_STRINGS[4].equalsIgnoreCase(comparisonString))
			return Comparison.NOT_EQUAL;
		if(COMPARISON_STRINGS[5].equalsIgnoreCase(comparisonString))
			return Comparison.GREATER_OR_EQUAL;
		if(COMPARISON_STRINGS[6].equalsIgnoreCase(comparisonString))
			return Comparison.GREATER;
		throw new ParseException("Unrecognised comparison", 0);
	}
	
	
	/**
	 * @param compareColumn
	 * @param comparison
	 * @param valueString
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws ParseException
	 */
	public static ComparantConstraint FromString(ComparableColumn<?> compareColumn, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return FromString(new ColumnPointer<ComparableColumn<?>>(compareColumn), comparison, valueString);
	}
	
	/**
	 * @param columnPointer
	 * @param comparison
	 * @param valueString
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws ParseException
	 */
	public static ComparantConstraint FromString(ColumnPointer<ComparableColumn<?>> columnPointer, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return new ComparantConstraint(columnPointer, comparison, columnPointer.getColumn().parse(valueString));
	}
	
	// DYNAMICS------------------------------------------------------
	private ComparingComparant<?, ?> lhs;
	private Comparison comparison;
	private Comparant<?> rhs;
	
	/**
	 * @param compareColumn must be a top-level column
	 * @param comparison
	 * @param rhsValue the value at the right-hand-side of the comparison
	 */
	public ComparantConstraint(ComparableColumn<?> compareColumn, Comparison comparison, Object rhsValue)
	{
		this(new ColumnPointer<ComparableColumn<?>>(compareColumn), comparison, rhsValue);
	}
	
	/**
	 * @param lhsColumnPointer the columnPointer at the left-hand-side of the comparison
	 * @param comparison
	 * @param rhsValue the value at the right-hand-side of the comparison
	 */
	public ComparantConstraint(ColumnPointer<ComparableColumn<?>> lhsColumnPointer, Comparison comparison, Object rhsValue)
	{
		if(rhsValue == null && comparison != Comparison.EQUAL && comparison != Comparison.NOT_EQUAL)
			throw new NullPointerException("Value cannot be null unless comparison is equality or inequality.");
		this.lhs = new ColumnComparant(lhsColumnPointer);
		this.comparison = comparison;
		this.rhs = new LiteralComparant<Object>(rhsValue);
	}
	
	/**
	 * RuleConstraint comparing the values between 2 ComparableColumns of the same type <C>
	 * 
	 * @param lhsColumn the column at the left-hand-side of the comparison, must be a top-level column
	 * @param rhsColumn the column at the right-hand-side of the comparison, must be a top-level column
	 * @param comparison
	 */
	public <C> ComparantConstraint(ComparableColumn<C> lhsColumn, ComparableColumn<C> rhsColumn, Comparison comparison)
	{
		this(new ColumnPointer<ComparableColumn<?>>(lhsColumn), new ColumnPointer<ComparableColumn<?>>(rhsColumn), comparison);
	}
	
	private ComparantConstraint(ColumnPointer<ComparableColumn<?>> lhsColumnPointer, ColumnPointer<ComparableColumn<?>> rhsColumnPointer, Comparison comparison)
	{
		this(new ColumnComparant(lhsColumnPointer), comparison, new ColumnComparant(rhsColumnPointer));
	}
	
	private <X extends Object> ComparantConstraint(ComparingComparant<?, X> lhs, Comparison comparison, Comparant<X> rhs)
	{
		this.lhs = lhs;
		this.comparison = comparison;
		this.rhs = rhs;
	}

	/**
	 * @return the comparison
	 */
	public Comparison getComparison()
	{
		return comparison;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean _isValid(Record record)
	{
		// Compare value:
		int compResult = 0; // TODO solve generics hell: lhs.compare(record, rhs);
		switch(comparison)
		{
			case SMALLER:
				return compResult < 0;
			case SMALLER_OR_EQUAL:
				return compResult <= 0;
			case EQUAL:
				return compResult == 0;
			case NOT_EQUAL:
				return compResult != 0;
			case GREATER_OR_EQUAL:
				return compResult >= 0;
			case GREATER:
				return compResult > 0;
			default:
				throw new IllegalStateException("Unknown comparison: " + comparison.name());
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		//visitor.visit(this);
	}


	@Override
	public boolean equals(Object obj)
	{
		// TODO Auto-generated method stub
		return false;
	}


	@Override
	public int hashCode()
	{
		// TODO Auto-generated method stub
		return 0;
	}
	
}