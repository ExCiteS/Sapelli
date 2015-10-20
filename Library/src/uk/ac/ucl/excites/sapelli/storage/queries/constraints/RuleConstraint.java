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
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Rule-based Constraint class
 * 
 * @author mstevens
 */
public class RuleConstraint extends Constraint
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
	public static <CC extends ComparableColumn<?>> RuleConstraint FromString(CC compareColumn, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return FromString(new ColumnPointer<CC>(compareColumn), comparison, valueString);
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
	public static RuleConstraint FromString(ColumnPointer<? extends ComparableColumn<?>> columnPointer, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return new RuleConstraint(columnPointer, comparison, columnPointer.getColumn().parse(valueString));
	}
	
	// DYNAMICS------------------------------------------------------
	private ColumnPointer<? extends ComparableColumn<?>> lhsColumnPointer;
	private Comparison comparison;
	private Object rhsValue;
	private ColumnPointer<? extends ComparableColumn<?>> rhsColumnPointer;
	
	/**
	 * @param compareColumn must be a top-level column
	 * @param comparison
	 * @param rhsValue the value at the right-hand-side of the comparison
	 */
	public <CC extends ComparableColumn<?>> RuleConstraint(CC compareColumn, Comparison comparison, Object rhsValue)
	{
		this(new ColumnPointer<CC>(compareColumn), comparison, rhsValue);
	}
	
	/**
	 * @param lhsColumnPointer the columnPointer at the left-hand-side of the comparison
	 * @param comparison
	 * @param rhsValue the value at the right-hand-side of the comparison
	 */
	public RuleConstraint(ColumnPointer<? extends ComparableColumn<?>> lhsColumnPointer, Comparison comparison, Object rhsValue)
	{
		if(rhsValue == null && comparison != Comparison.EQUAL && comparison != Comparison.NOT_EQUAL)
			throw new NullPointerException("Value cannot be null unless comparison is equality or inequality.");
		this.lhsColumnPointer = lhsColumnPointer;
		this.comparison = comparison;
		this.rhsValue = rhsValue;
	}
	
	/**
	 * RuleConstraint comparing the values between 2 ComparableColumns of the same type <C>
	 * 
	 * @param lhsColumn the column at the left-hand-side of the comparison, must be a top-level column
	 * @param rhsColumn the column at the right-hand-side of the comparison, must be a top-level column
	 * @param comparison
	 */
	public <CCL extends ComparableColumn<C>, CCR extends ComparableColumn<C>, C> RuleConstraint(CCL lhsColumn, CCR rhsColumn, Comparison comparison)
	{
		this(new ColumnPointer<CCL>(lhsColumn), new ColumnPointer<CCR>(rhsColumn), comparison);
	}
	
	private <CC extends ComparableColumn<C>, C> RuleConstraint(ColumnPointer<? extends ComparableColumn<?>> lhsColumnPointer, ColumnPointer<? extends ComparableColumn<?>> rhsColumnPointer, Comparison comparison)
	{
		this.lhsColumnPointer = lhsColumnPointer;
		this.comparison = comparison;
		this.rhsColumnPointer = rhsColumnPointer;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#negate()
	 */
	@Override
	public RuleConstraint negate()
	{
		Comparison negatedComparison = comparison.negate(); // e.g. NOT (col <= x) --> col > x
		return rhsColumnPointer != null ?
			new RuleConstraint(	lhsColumnPointer,
								rhsColumnPointer,
								negatedComparison) :
			new RuleConstraint(	lhsColumnPointer,
								negatedComparison,
								rhsValue);
	}

	/**
	 * @return the left-hand-side columnPointer
	 */
	public ColumnPointer<? extends ComparableColumn<?>> getLHSColumnPointer()
	{
		return lhsColumnPointer;
	}

	/**
	 * @return the left-hand-side ComparableColumn
	 */
	public ComparableColumn<?> getLHSCompareColumn()
	{
		return lhsColumnPointer.getColumn();
	}
	
	/**
	 * @return the right-hand-side columnPointer
	 */
	public ColumnPointer<? extends ComparableColumn<?>> getRHSColumnPointer()
	{
		return rhsColumnPointer;
	}

	/**
	 * @return the right-hand-side ComparableColumn
	 */
	public ComparableColumn<?> getRHSCompareColumn()
	{
		return isRHSColumn() ? rhsColumnPointer.getColumn() : null;
	}

	/**
	 * @return the comparison
	 */
	public Comparison getComparison()
	{
		return comparison;
	}

	/**
	 * @return the value
	 */
	public Object getRHSValue()
	{
		return rhsValue;
	}
	
	/**
	 * @return whether the right-hand-side of the comparison is a column ({@code true}) or a literal value ({@code false})
	 */
	public boolean isRHSColumn()
	{
		return rhsColumnPointer != null;
	}
	
	/**
	 * @return whether the right-hand-side of the comparison is a literal value ({@code true}) or a column ({@code false})
	 */
	public boolean isRHSValue()
	{
		return !isRHSColumn();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean _isValid(Record record)
	{
		// Get (sub)record(s) and the rhs value:
		Object theRhsValue = rhsValue;
		ValueSet<?> lhsRecord = lhsColumnPointer.getValueSet(record, false);
		if(lhsRecord == null)
			return false;
		if(isRHSColumn())
		{
			ValueSet<?> rhsRecord = rhsColumnPointer.getValueSet(record, false);
			if(rhsRecord == null)
				return false;
			theRhsValue = getRHSCompareColumn().retrieveValue(rhsRecord); // get rhsValue from rhsColumn
		}
		// Compare value:
		int compResult = getLHSCompareColumn().retrieveAndCompareToObject(lhsRecord, theRhsValue);
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
		visitor.visit(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof RuleConstraint)
		{
			RuleConstraint that = (RuleConstraint) obj;
			return	this.lhsColumnPointer.equals(that.lhsColumnPointer) &&
					this.comparison == that.comparison &&
					(this.rhsValue != null ? this.rhsValue.equals(that.rhsValue) : that.rhsValue == null) &&
					(this.rhsColumnPointer != null ? this.rhsColumnPointer.equals(that.rhsColumnPointer) : that.rhsColumnPointer == null);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + lhsColumnPointer.hashCode();
		hash = 31 * hash + comparison.ordinal();
		hash = 31 * hash + (rhsValue != null ? rhsValue.hashCode() : 0);
		hash = 31 * hash + (rhsColumnPointer != null ? rhsValue.hashCode() : 0);
		return hash;
	}
	
}