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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.LabelUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * @author mstevens
 *
 */
public class LabelField extends Field
{

	static public final String ID_PREFIX = "lbl";
	static public final float DEFAULT_TEXT_SIZE_SCALE = 1.0f; // same as surrounding text  
	static public final boolean DEFAULT_TEXT_CENTERED = false;
	
	private float textSizeScale = DEFAULT_TEXT_SIZE_SCALE;
	private boolean centered = DEFAULT_TEXT_CENTERED;
	
	/**
	 * @param form
	 * @param id the id, may be null
	 * @param caption the label's caption, cannot be null.
	 * @throws NullPointerException when the caption is null
	 */
	public LabelField(Form form, String id, String caption) throws NullPointerException
	{	
		super(	form,
				GetID(id, form, ID_PREFIX, caption),
				CheckCaption(caption));
		this.noColumn = true;
		this.optional = true;
	}
	
	/**
	 * @return the textSizeScale
	 */
	public float getTextSizeScale()
	{
		return textSizeScale;
	}

	/**
	 * @param textSizeScale the textSizeScale to set
	 */
	public void setTextSizeScale(float textSizeScale)
	{
		this.textSizeScale = textSizeScale;
	}

	/**
	 * @return the centered
	 */
	public boolean isCentered()
	{
		return centered;
	}

	/**
	 * @param centered the centered to set
	 */
	public void setCentered(boolean centered)
	{
		this.centered = centered;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
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
		return visitor.enterLabelField(this, arguments, withPage);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#createUI(uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> LabelUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createLabelUI(this);
	}
	
	@Override
	public void setNoColumn(boolean noColumn)
	{
		//form.addWarning("Ignored noColumn setting on Label (it never has a column)");
		//throw new UnsupportedOperationException("setNoColumn is unsupported on LabelFields since they never have columns.");
	}
	
	@Override
	public void setOptional(boolean optional)
	{
		// does nothing, labels are always optional
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof LabelField)
		{
			LabelField that = (LabelField) obj;
			return	super.equals(that) && // Field#equals(Object)
					this.textSizeScale == that.textSizeScale &&
					this.centered == that.centered;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + Float.floatToIntBits(textSizeScale);
		hash = 31 * hash + (centered ? 0 : 1);
		return hash;
	}
	
}