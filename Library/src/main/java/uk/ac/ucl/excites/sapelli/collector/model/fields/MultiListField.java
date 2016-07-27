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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary.DictionarySerialiser;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.DictionaryItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.MultiListUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.StringListMapper;

/**
 * Field that allows users to select a value from a hierarchy presented as multiple listboxes
 * 
 * @author mstevens
 */
public class MultiListField extends Field
{

	static public final String UNKNOWN_LABEL_PREFIX = "Level"; //TODO multilang
	static public final boolean DEFAULT_PRESELECT = true;
	static public final String CAPTION_SEPARATOR = ";";
	
	private final String[] captions;
	private final MultiListItem itemsRoot;
	private boolean preSelect = DEFAULT_PRESELECT;
	private final Dictionary<MultiListItem> values;
	
	/**
	 * @param form
	 * @param id
	 * @param captions
	 */
	public MultiListField(Form form, String id, String captions)
	{
		super(form, id);
		this.captions = captions.split(CAPTION_SEPARATOR, -1); // -1: allow empty Strings
		this.itemsRoot = new MultiListItem(this);
		this.values = new Dictionary<MultiListItem>();
	}
	
	@Override
	public String getCaption()
	{
		return getCaption(0);
	}
	
	public String getCaption(int level)
	{
		if(level < 0)
			throw new IndexOutOfBoundsException("Level cannot be negative!");
		else if(level < captions.length)
			return captions[level];
		else
			return captions[captions.length - 1].isEmpty() ? "" : UNKNOWN_LABEL_PREFIX + level; // if last existing caption is "" then return "", otherwise return "LevelX"
	}

	/**
	 * @return the preSelect
	 */
	public boolean isPreSelect()
	{
		return preSelect;
	}

	/**
	 * @param preSelect the preSelect to set
	 */
	public void setPreSelect(boolean preSelect)
	{
		this.preSelect = preSelect;
	}

	public MultiListItem getItemsRoot()
	{
		return itemsRoot;
	}
	
	/**
	 * @return the values dictionary
	 */
	public Dictionary<MultiListItem> getDictionary()
	{
		return values;
	}
	
	public int getValueForItem(MultiListItem item)
	{
		return values.lookupIndex(item);
	}
	
	public MultiListItem getItemForValue(int value)
	{
		return values.lookupItem(value);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn(String)
	 */
	@Override
	protected IntegerColumn createColumn(String name)
	{
		// Initialise dictionary:
		addLeaves(itemsRoot); // depth-first traversal
		// Column...
		if(values.isEmpty())
		{	//no values set
			noColumn = true; //!!!
			form.addWarning("noColumn was forced to true on MultiListField " + id + " because it has no items.");
			return null;
		}
		else
		{
			boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
			
			//Create column:
			IntegerColumn col = new IntegerColumn(name, colOptional, 0, values.size() - 1, true); // Allow empty! For when values.size() = 1
			
			// Add virtual columns to it:
			//	Find maximum level:
			int maxLevel = 0;
			for(MultiListItem item : values.getItems())
			{
				int itemLevel = item.getLevel(); 
				if(itemLevel > maxLevel)
					maxLevel = itemLevel;
			}
			//	A String value column for each level: 
			for(int l = 0; l <= maxLevel; l++)
			{
				final int level = l;
				//	Value String column:
				StringListMapper levelValueMapper = new StringListMapper(values.serialise(new DictionarySerialiser<MultiListItem>()
				{
					@Override
					public String serialise(MultiListItem item)
					{
						MultiListItem parentAtLevel = item.getParentAt(level);
						return parentAtLevel != null ? parentAtLevel.value : null;
					}
				}));
				String vColName = getCaption(level).trim().isEmpty() ? (name + '_' + l) : Column.SanitiseName(getCaption(level).trim()); // Remove any illegal chars in caption before using it as column name
				col.addVirtualVersion(StringColumn.ForCharacterCount(vColName, colOptional, Math.max(levelValueMapper.getMaxStringLength(), 1)), levelValueMapper);
			}

			// Return the column:
			return col;
		}
	}
	
	private void addLeaves(MultiListItem item)
	{
		if(item.isLeaf())
			values.addItem(item);
		else
			for(MultiListItem c : item.getChildren())
				addLeaves(c);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterMultiListField(this, arguments, withPage);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public <V, UI extends CollectorUI<V, UI>> MultiListUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createMultiListUI(this);
	}
	
	@Override
	public IntegerColumn getColumn()
	{
		return (IntegerColumn) super.getColumn();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof MultiListField)
		{
			MultiListField that = (MultiListField) obj;
			return	super.equals(that) && // Field#equals(Object)
					Arrays.equals(this.captions, that.captions) &&
					this.itemsRoot.equals(that.itemsRoot) &&
					this.preSelect == that.preSelect;
					// Do not include the values dictionary here, it is unnecessary and may cause endless loops.
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + Arrays.hashCode(captions);
		hash = 31 * hash + itemsRoot.hashCode();
		hash = 31 * hash + (preSelect ? 0 : 1);
		// Do not include the values dictionary here, it is unnecessary and may cause endless loops.
		return hash;
	}
	
	/**
	 * A class representing items in the MultiListField
	 * 
	 * @author mstevens
	 */
	public static class MultiListItem implements DictionaryItem
	{
		
		static public final int NO_DEFAULT_ITEM_SET_IDX = -1; 

		static public MultiListItem GetDummyItem(MultiListField field, String value)
		{
			MultiListItem dummy = new MultiListItem(field);
			dummy.value = value;
			return dummy;
		}
		
		private final MultiListField field;
		private final MultiListItem parent;
		
		/**
		 * Used to avoid cases in which siblings with the same attributes would produce the 
		 * same {@link #hashCode()} and would be treated as equal by {@link #equals(Object)},
		 * and therefore be represented by only a single dictionary entry.
		 */
		private int position;
		private String value;
		
		private List<MultiListItem> children;
		private int defaultChildIdx = NO_DEFAULT_ITEM_SET_IDX;

		/**
		 * Only for root item held directly by MultiListField
		 * 
		 * @param field
		 */
		/*package*/ MultiListItem(MultiListField field)
		{
			if(field == null)
				throw new NullPointerException("field cannot be null!");
			this.field = field;
			this.parent = null;
			this.value = null;
		}
		
		public MultiListItem(MultiListItem parent, String value)
		{
			if(parent == null)
				throw new IllegalArgumentException("Parent cannot be null");
			this.parent = parent;
			parent.addChild(this); //!!!
			this.field = parent.field;
			this.value = value;
		}
		
		/**
		 * @return the value
		 */
		public String getValue()
		{
			return value;
		}
		
		public String toString()
		{
			return value;
		}
		
		public void addChild(MultiListItem child)
		{
			if(children == null)
				children = new ArrayList<MultiListItem>();
			child.position = children.size();
			children.add(child);
		}
		
		/**
		 * @return the field
		 */
		public MultiListField getField()
		{
			return field;
		}

		/**
		 * @return the parent
		 */
		public MultiListItem getParent()
		{
			return parent;
		}
		
		public boolean isRoot()
		{
			return parent == null;
		}
		
		/**
		 * @return the children
		 */
		public List<MultiListItem> getChildren()
		{
			return children != null ? children : Collections.<MultiListItem> emptyList();
		}
		
		public boolean isLeaf()
		{
			return children == null;
		}
		
		/**
		 * @return the level of the item (levels start at 0 and correspond to captions)
		 */
		public int getLevel()
		{
			if(isRoot())
				return -1; // root doesn't count as an actual level
			else
				return 1 + parent.getLevel();
		}
		
		public MultiListItem getParentAt(int level)
		{
			int myLevel = getLevel();
			if(myLevel == level)
				return this;
			else if(myLevel < level)
				return null;
			else //if(myLevel > level)
				return parent.getParentAt(level); // go up
		}
		
		/**
		 * @return the defaultChild
		 */
		public MultiListItem getDefaultChild()
		{
			if(defaultChildIdx == NO_DEFAULT_ITEM_SET_IDX || isLeaf())
				return null;
			else
				return children.get(defaultChildIdx);
		}
		
		/**
		 * @return the index of the defaultChild
		 */
		public int getDefaultChildIndex()
		{
			return defaultChildIdx;
		}

		/**
		 * @param defaultChild the defaultChild to set
		 */
		public void setDefaultChild(MultiListItem defaultChild)
		{
			if(isLeaf())
				throw new IllegalArgumentException("Unknown child: " + defaultChild.toString());
			int idx = children.indexOf(defaultChild);
			if(idx == -1)
				throw new IllegalArgumentException("Unknown child: " + defaultChild.toString());
			this.defaultChildIdx = idx;
		}

		@Override
		public List<String> getDocExtras()
		{
			return null;
		}
		
		@Override
		public boolean equals(Object obj)
		{
			if(this == obj)
				return true; // references to same object
			if(obj instanceof MultiListItem)
			{
				MultiListItem that = (MultiListItem) obj;
				return	this.field.id.equals(that.field.id) &&
						(this.parent != null ? that.parent != null && (this.parent.value != null ? this.parent.value.equals(that.parent.value) : that.parent.value == null) : that.parent == null) &&
						this.position == that.position &&
						(this.value != null ? this.value.equals(that.value) : that.value == null) &&
						this.getChildren().equals(that.getChildren()) &&
						this.defaultChildIdx == that.defaultChildIdx;
			}
			else
				return false;
		}
		
		@Override
		public int hashCode()
		{
			int hash = 1;
			hash = 31 * hash + field.id.hashCode();
			hash = 31 * hash + (parent != null ? (parent.value != null ? parent.value.hashCode() : 1) : 0); // do not use parent.hashCode() (to avoid endless loop)
			hash = 31 * hash + position;
			hash = 31 * hash + (value != null ? value.hashCode() : 0);
			hash = 31 * hash + getChildren().hashCode();
			hash = 31 * hash + defaultChildIdx;
			return hash;
		}

	}
	
}
