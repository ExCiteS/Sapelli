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

package uk.ac.ucl.excites.sapelli.storage.visitors;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn.Simple;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;

/**
 * A {@link ColumnVisitor} that treats all Column types the same.
 * Note: the visiting of VirtualColumns & the entering/leaving of RecordColumns is left to subclasses.
 * 
 * @author mstevens
 */
public abstract class SimpleColumnVisitor implements ColumnVisitor
{

	/**
	 * One method to deal with all types of columns
	 * 
	 * @param leafColumn
	 */
	public abstract <T> void visit(Column<T> Column);
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn)
	 */
	@Override
	public void visit(BooleanColumn boolCol)
	{
		this.visit((Column<?>) boolCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn)
	 */
	@Override
	public void visit(TimeStampColumn timeStampCol)
	{
		this.visit((Column<?>) timeStampCol);	
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn)
	 */
	@Override
	public void visit(FloatColumn floatCol)
	{
		this.visit((Column<?>) floatCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn)
	 */
	@Override
	public void visit(ForeignKeyColumn foreignKeyCol)
	{
		this.visit((Column<?>) foreignKeyCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn)
	 */
	@Override
	public void visit(IntegerColumn intCol)
	{
		this.visit((Column<?>) intCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn)
	 */
	@Override
	public void visit(IntegerListColumn intListCol)
	{
		this.visit((Column<?>) intListCol);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn)
	 */
	@Override
	public void visit(BooleanListColumn boolListCol)
	{
		this.visit((Column<?>) boolListCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn)
	 */
	@Override
	public void visit(LineColumn lineCol)
	{
		this.visit((Column<?>) lineCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn)
	 */
	@Override
	public void visit(LocationColumn locCol)
	{
		this.visit((Column<?>) locCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn)
	 */
	@Override
	public void visit(OrientationColumn orCol)
	{
		this.visit((Column<?>) orCol);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn)
	 */
	@Override
	public void visit(ByteArrayColumn byteArrayCol)
	{
		this.visit((Column<?>) byteArrayCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn)
	 */
	@Override
	public void visit(PolygonColumn polyCol)
	{
		this.visit((Column<?>) polyCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn)
	 */
	@Override
	public void visit(StringColumn stringCol)
	{
		this.visit((Column<?>) stringCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.ListColumn.Simple)
	 */
	@Override
	public <T> void visit(Simple<T> simpleListCol)
	{
		this.visit((Column<?>) simpleListCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn)
	 */
	@Override
	public <VT, ST> void visit(VirtualColumn<VT, ST> virtCol)
	{
		this.visit((Column<?>) virtCol);
	}
	
}
