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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.Next;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * Dummy field to represent the end of any form.
 * 
 * @author mstevens
 */
public class EndField extends UILessField
{
	// Backwards compatibility with v1.x projects:
	static public final String END = "_END";
	static public final String CANCEL = "_CANCEL";
	
	static public List<EndField> GetEndFields(Form form)
	{
		List<EndField> efs = new ArrayList<EndField>();

		// v2.x "End jumps":
		for(Next nxt : Next.values())
		{
			efs.add(new EndField(form, true, nxt));
			efs.add(new EndField(form, false, nxt));
		}
		// v1.x compatibility:
		// 	Form default _END:
		efs.add(new EndField(form, true));
		// 	_CANCEL:
		efs.add(new EndField(form, false));

		return efs;
	}
	
	// Dynamics
	private final boolean save;
	private final Next next;
	
	public EndField(Form form, boolean save, Next next)
	{
		super(form, '_' + (save ? "SAVE+" : "") + next.name());
		this.noColumn = true;
		this.save = save;
		this.next = next;
	}
	
	/**
	 * Creates the default EndField for the form, which will always save the record and then to the next behaviour specified by the form's "next"
	 * For v1.x compatibility only
	 * 
	 * @param form
	 * @param end	whether this is an _END field (true) or an _CANCEL field (false)
	 */
	private EndField(Form form, boolean end)
	{
		super(form, end ? END : CANCEL);
		this.next = end ? form.getNext() : Next.LOOPFORM;
		this.save = end;
	}
	
	/**
	 * @return the save
	 */
	public boolean isSave()
	{
		return save;
	}

	/**
	 * @return the next
	 */
	public Next getNext()
	{
		return next;
	}

	@Override
	protected Column<?> createColumn(String name)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterEndField(this, arguments);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof EndField)
		{
			EndField that = (EndField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.save == that.save &&
					this.next == that.next;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (save ? 0 : 1);
		hash = 31 * hash + next.ordinal();
		return hash;
	}

}
