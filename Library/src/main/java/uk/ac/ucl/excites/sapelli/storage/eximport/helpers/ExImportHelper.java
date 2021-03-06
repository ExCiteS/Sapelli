/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.storage.eximport.helpers;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.types.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.SimpleColumnVisitor;

/**
 * Abstract super class for {@link ExportHelper} and {@link ImportHelper} which
 * are helper classes intended for respectively export or importing {@link Record}s to or from export formats
 * which have their own way of differentiating between {@code null} and empty List/String values.
 * The {@link ExportHelper} converts Column values into String representation (for exporting),
 * and the {@link ImportHelper} converts String representation to Column values (for importing).
 * 
 * The process works by calling a {@link #inspect(Column)} with a column to be inspected. This is not necessarily
 * a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such,
 * meaning it will be inspected as a whole. It is up to the exporter/importer using this class to decide which/whether
 * ValueSetColumns are broken up (with their constituent subcolumns inspected one by one, with multiple calls to
 * {@link #inspect(Column)}) or treated as an "atomic" whole (with the ValueSetColumn subclass instance inspected
 * by means of as single call to {@link #inspect(Column)}).
 * 
 * There is special behaviour to deal with {@link ListLikeColumn}s (i.e. {@link StringColumn}s and {@link ListColumn}s)
 * whose own serialisation delimiters are not used because the export format has its own way of differentiating between
 * {@code null} and empty List/String values.
 * 
 * All {@link ListColumn} subclasses are treated the same, namely as {@link ListLikeColumn}s.
 * 
 * @see ExportHelper
 * @see ImportHelper
 * 
 * @author mstevens
 */
public abstract class ExImportHelper
{
	
	private final ColumnInspector inspector = new ColumnInspector();

	/**
	 * @param column should not be null! This is not necessarily a "leaf" column (i.e. it may be an instance of a {@link ValueSetColumn} subclass) but it will be treated as such, meaning it will be inspected as a whole and not broken up.
	 */
	protected final <T> void inspect(Column<T> column)
	{
		column.accept(inspector); // Visit column with inspector
	}
	
	/**
	 * Used for all {@link Column}s except {@link ValueSetColumn}s and {@link ListLikeColumn}s.
	 * 
	 * @param column
	 */
	protected abstract <T> void visit(Column<T> column);
	
	/**
	 * Used for all {@link ValueSetColumn}s.
	 * 
	 * @param valueSetCol
	 */
	protected abstract <VS extends ValueSet<CS>, CS extends ColumnSet> void visit(ValueSetColumn<VS, CS> valueSetCol);
	
	/**
	 * Used for all {@link ListLikeColumn}s.
	 * 
	 * @param listLikeColumn
	 */
	protected abstract <T> void visit(ListLikeColumn<T> listLikeColumn);
	
	/**
	 * 
	 * @return whether or not to deal with VirtualColumns by visiting their targets
	 */
	protected abstract boolean visitVirtualColumnTargets();
	
	/**
	 * The actual "inspector", implemented as a {@link SimpleColumnVisitor}.
	 * 
	 * @author mstevens
	 */
	private final class ColumnInspector extends SimpleColumnVisitor
	{
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.Column)
		 */
		@Override
		protected final <T> void visit(Column<T> column)
		{
			ExImportHelper.this.visit(column);
		}
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn)
		 */
		@Override
		public final void visit(StringColumn stringCol)
		{
			ExImportHelper.this.visit((ListLikeColumn<String>) stringCol); // treat as a ListLikeColumn!
		}
		
		/**
		 * Visit method used for all {@link ListColumn}s.
		 * 
		 * @param listCol
		 */
		public final <L extends List<T>, T> void visitListColumn(ListColumn<L, T> listCol)
		{
			ExImportHelper.this.visit((ListLikeColumn<L>) listCol); // treat as a ListLikeColumn!
		}
	
		/**
		 * Visit method used for all {@link ValueSetColumn}s.
		 * 
		 * @param valueSetCol
		 */
		public final <VS extends ValueSet<CS>, CS extends ColumnSet> void visitValueSetColumn(ValueSetColumn<VS, CS> valueSetCol)
		{
			ExImportHelper.this.visit(valueSetCol);
		}
		
		/**
		 * This method is only called is includeVirtualColumns() (below) returns true.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SimpleColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn)
		 */
		@Override
		public final <VT, ST> void visit(VirtualColumn<VT, ST> virtCol)
		{
			virtCol.getTargetColumn().accept(this); // visit the target column!
		}
		
		@Override
		public final boolean includeVirtualColumns()
		{
			return ExImportHelper.this.visitVirtualColumnTargets();
		}
	
		@Override
		public final void visit(ForeignKeyColumn foreignKeyCol)
		{
			visitValueSetColumn(foreignKeyCol);
		}

		@Override
		public final void visit(LocationColumn locCol)
		{
			visitValueSetColumn(locCol);
		}

		@Override
		public final void visit(OrientationColumn orCol)
		{
			visitValueSetColumn(orCol);
		}

		@Override
		public final <T> void visit(ListColumn.Simple<T> simpleListCol)
		{
			visitListColumn(simpleListCol);
		}
		
		@Override
		public final void visit(IntegerListColumn intListCol)
		{
			visitListColumn(intListCol);
		}
		
		@Override
		public final void visit(BooleanListColumn boolListCol)
		{
			visitListColumn(boolListCol);
		}
		
		@Override
		public final void visit(StringListColumn stringListCol)
		{
			visitListColumn(stringListCol);
		}
		
		@Override
		public final void visit(ByteArrayListColumn byteArrayListCol)
		{
			visitListColumn(byteArrayListCol);
		}
		
		@Override
		public final void visit(PolygonColumn polyCol)
		{
			visitListColumn(polyCol);
		}
		
		@Override
		public final void visit(LineColumn lineCol)
		{
			visitListColumn(lineCol);
		}
		
		/**
		 * Not applicable, see {@link ExImportHelper#inspect(Column)}.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
		 */
		@Override
		public final <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(ValueSetColumn<VS, CS> valueSetCol)
		{
			// do nothing (is never called)
		}
	
		/**
		 * Not applicable, see {@link ExImportHelper#inspect(Column)}.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#leave(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
		 */
		@Override
		public final <VS extends ValueSet<CS>, CS extends ColumnSet> void leave(ValueSetColumn<VS, CS> valueSetCol)
		{
			// do nothing (is never called)
		}
	
		/**
		 * Always visit as a whole, see {@link ExImportHelper#inspect(Column)}.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#splitLocationTraversal()
		 */
		@Override
		public final boolean splitLocationTraversal()
		{
			return false; // treat as a whole
		}
	
		/**
		 * Always visit as a whole, see {@link ExImportHelper#inspect(Column)}.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#splitOrientationTraversal()
		 */
		@Override
		public final boolean splitOrientationTraversal()
		{
			return false; // treat as a whole
		}
	
		/**
		 * Always visit as a whole, see {@link ExImportHelper#inspect(Column)}.
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#splitForeignKeyTraversal()
		 */
		@Override
		public final boolean splitForeignKeyTraversal()
		{
			return false; // treat as a whole
		}
	
		@Override
		public final boolean skipNonBinarySerialisedLocationSubColumns()
		{
			return false; // not applicable, we always export/import all subcolumns
		}
	
		@Override
		public final boolean skipNonBinarySerialisedOrientationSubColumns()
		{
			return false; // not applicable, we always export/import all subcolumns
		}
	
	}
	
}
