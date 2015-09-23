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

import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
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
 * TODO document: mainly to explain how traversal of valuesetcolumns (and subclasses) works
 * 
 * @author mstevens
 */
public interface ColumnVisitor
{
	
	public void visit(BooleanColumn boolCol);
	
	public void visit(TimeStampColumn timeStampCol);
	
	public void visit(FloatColumn floatCol);
	
	public void visit(IntegerColumn intCol);
		
	public void visit(StringColumn stringCol);
	
	public void visit(IntegerListColumn intListCol);
	
	public void visit(LineColumn lineCol);
	
	public void visit(PolygonColumn polyCol);
	
	public void visit(ForeignKeyColumn foreignKeyCol);
	
	public void visit(LocationColumn locCol);
	
	public void visit(OrientationColumn orCol);
	
	public void visit(ByteArrayColumn byteArrayCol);
	
	public <T> void visit(ListColumn.Simple<T> simpleListCol);
	
	public <VT, ST> void visit(VirtualColumn<VT, ST> virtCol);
	
	public void enter(ValueSetColumn<?> recordCol);
	
	public void leave(ValueSetColumn<?> recordCol);
	
	public boolean allowLocationSelfTraversal();
	
	public boolean allowOrientationSelfTraversal();
	
	public boolean allowForeignKeySelfTraversal();
	
	public boolean skipNonBinarySerialisedLocationSubColumns();
	
	public boolean skipNonBinarySerialisedOrientationSubColumns();
	
	public boolean includeVirtualColumns();
	
}
