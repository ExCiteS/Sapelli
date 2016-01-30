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

package uk.ac.ucl.excites.sapelli.storage.visitors;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.types.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;

/**
 * An interface for visitors that inspect columns.
 * It has {@code visit(Column)} methods for each type of column.
 * Additionally the {@code enter(ValueSetColumn)} and an {@code leave(ValueSetColumn)} methods are used to signal
 * that the visitor is respectively entering or leaving a {@link ValueSetColumn} to inspect its subcolumns. 
 *
 * @see <a href="https://en.wikipedia.org/wiki/Visitor_pattern">Visitor Design Pattern</a>
 * @author mstevens
 */
public interface ColumnVisitor
{
	
	public void visit(BooleanColumn boolCol);
	
	public void visit(TimeStampColumn timeStampCol);
	
	public void visit(FloatColumn floatCol);
	
	public void visit(IntegerColumn intCol);
	
	public void visit(StringColumn stringCol);
	
	public void visit(ByteArrayColumn byteArrayCol);
	
	public void visit(IntegerListColumn intListCol);
	
	public void visit(BooleanListColumn boolListCol);
	
	public void visit(StringListColumn stringListCol);
	
	public void visit(ByteArrayListColumn byteArrayListCol);
	
	public void visit(LineColumn lineCol);
	
	public void visit(PolygonColumn polyCol);
	
	public void visit(ForeignKeyColumn foreignKeyCol);
	
	public void visit(LocationColumn locCol);
	
	public void visit(OrientationColumn orCol);
	
	public <T> void visit(ListColumn.Simple<T> simpleListCol);
	
	public <VT, ST> void visit(VirtualColumn<VT, ST> virtCol);
	
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(ValueSetColumn<VS, CS> valueSetCol);
	
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void leave(ValueSetColumn<VS, CS> valueSetCol);
	
	public boolean splitLocationTraversal();
	
	public boolean splitOrientationTraversal();
	
	public boolean splitForeignKeyTraversal();
	
	public boolean skipNonBinarySerialisedLocationSubColumns();
	
	public boolean skipNonBinarySerialisedOrientationSubColumns();
	
	public boolean includeVirtualColumns();
	
}
