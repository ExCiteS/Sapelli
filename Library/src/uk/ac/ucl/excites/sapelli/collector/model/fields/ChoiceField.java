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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.Dictionary.DictionarySerialiser;
import uk.ac.ucl.excites.sapelli.collector.model.dictionary.DictionaryItem;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.ChoiceUI;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.StringListMapper;


/**
 * Each ChoiceField represents a node in a decision tree. The whole of such a tree (starting with the root) describes the possible values the field (stored as an IntegerColumn) can take.
 * 
 * @author mstevens
 */
public class ChoiceField extends Field implements DictionaryItem
{
	
	static public final int DEFAULT_NUM_COLS = 1;
	static public final int DEFAULT_NUM_ROWS = 2;
	static public final String DEFAULT_ALT_TEXT = "?";
	static public final boolean DEFAULT_CROSSED = false;
	static public final String DEFAULT_CROSS_COLOR = "#A5FF0000"; // Red with 65% alpha
	
	static public final String IMAGE_VIRTUAL_COLOMN_TARGET_NAME = "Image";
	static public final String VALUE_VIRTUAL_COLOMN_TARGET_NAME = "Value";
	
	private ChoiceField parent;
	private ChoiceField root;
	private List<ChoiceField> children;
	private String imageRelativePath;
	private String answerDesc;
	private String questionDesc;
	private int cols = DEFAULT_NUM_COLS;
	private int rows = DEFAULT_NUM_ROWS;
	private String altText;
	private boolean crossed = DEFAULT_CROSSED;
	private String crossColor = DEFAULT_CROSS_COLOR;
	private final String value;
	private final ChoiceDictionary dictionary;
	
	/**
	 * @param form the form this choice(tree) belongs to
	 * @param id the id of the choice, only allowed to be null if not a root choice
	 * @param value the value of the choice (may be null)
	 * @param parent the parent of the choice (may be null if this is a root choice)
	 * @param caption the caption of the choicefield (may be null)
	 */
	public ChoiceField(Form form, String id, String value, ChoiceField parent, String caption)
	{
		super(	form,
				id == null || id.isEmpty() ?
					(parent == null ?
						null /* id is mandatory for the root: Field constructor will throw NullPointerException */ :
						/* generate id based on parent ID and value or child number: */
						parent.getID() + "." + (value == null || value.trim().isEmpty() ? parent.getChildren().size() + 1 : StringUtils.replaceWhitespace(value.trim(), "_"))) :
					id,
				caption);
		this.parent = parent;
		this.value = ((value == null || value.isEmpty()) ? null : value); //replace empty string with null (so we don't need to check for empty string elsewhere)
		if(parent == null)
		{	//this is a root choice
			root = this; //self-pointer
			dictionary = new ChoiceDictionary(); //root holds the dictionary
		}
		else
		{	//this is a child choice
			parent.addChild(this); //add myself as a child of my parent
			root = parent.root;
			dictionary = root.dictionary; //children share the dictionary of the root (so there is only 1 instance per choice tree)
		}
	}
	
	public void addChild(ChoiceField c)
	{
		if(children == null)
			children = new ArrayList<ChoiceField>();
		children.add(c);
	}

	/**
	 * @return the imageRelativePath
	 */
	public String getImageRelativePath()
	{
		return imageRelativePath;
	}

	/**
	 * @param imageRelativePath the imageRelativePath to set
	 */
	public void setImageRelativePath(String imageRelativePath)
	{
		this.imageRelativePath = imageRelativePath;
	}
	
	public boolean hasImage()
	{
		return imageRelativePath != null;
	}

	/**
	 * @return the answerDesc
	 */
	public String getAnswerDesc()
	{
		return answerDesc;
	}

	/**
	 * @param answerDesc
	 *            the answerDesc to set
	 */
	public void setAnswerDesc(String answerDesc)
	{
		this.answerDesc = answerDesc;
	}

	public boolean hasAudioAnswerDesc()
	{
		return answerDesc != null && FileHelpers.isAudioFileName(answerDesc);
	}

	/**
	 * @return the questionDesc
	 */
	public String getQuestionDesc()
	{
		return questionDesc;
	}

	/**
	 * @param questionDesc
	 *            the questionDesc to set
	 */
	public void setQuestionDesc(String questionDesc)
	{
		this.questionDesc = questionDesc;
	}

	public boolean hasQuestionDesc()
	{
		return questionDesc != null && FileHelpers.isAudioFileName(questionDesc);
	}

	/**
	 * @return the altText
	 */
	public String getAltText()
	{
		if(altText != null)
			return altText;
		if(value != null)
			return value;
		if(imageRelativePath != null)
			return imageRelativePath;
		return DEFAULT_ALT_TEXT;
	}

	/**
	 * @param altText the altText to set
	 */
	public void setAltText(String altText)
	{
		this.altText = altText;
	}
	
	/**
	 * @return the parent
	 */
	public ChoiceField getParent()
	{
		return parent;
	}
	
	/**
	 * Returns the root of this choice tree. This can be the same object (i.e. 'this') if it is the root.
	 * 
	 * @return the root
	 */
	@Override
	public ChoiceField getRoot()
	{
		return root;
	}
	
	/** Always return the caption of the root
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#getCaption()
	 */
	@Override
	public String getCaption()
	{
		return root.caption;
	}
	
	/**
	 * @return the children
	 */
	public List<ChoiceField> getChildren()
	{
		return children != null ? children : Collections.<ChoiceField> emptyList();
	}

	/**
	 * @return the cols
	 */
	public int getCols()
	{
		return cols;
	}

	/**
	 * @param cols the cols to set
	 */
	public void setCols(int cols)
	{
		this.cols = cols;
	}

	/**
	 * @return the rows
	 */
	public int getRows()
	{
		return rows;
	}

	/**
	 * @param rows the rows to set
	 */
	public void setRows(int rows)
	{
		this.rows = rows;
	}
	
	/**
	 * @return the crossed
	 */
	public boolean isCrossed()
	{
		return crossed;
	}

	/**
	 * @param crossed the crossed to set
	 */
	public void setCrossed(boolean crossed)
	{
		this.crossed = crossed;
	}

	/**
	 * @return the crossColor
	 */
	public String getCrossColor()
	{
		return crossColor;
	}

	/**
	 * @param crossColor the crossColor to set
	 */
	public void setCrossColor(String crossColor)
	{
		if(crossColor == null || crossColor.isEmpty())
			throw new IllegalArgumentException("crossColor cannot be null or empty");
		this.crossColor = crossColor;
	}

	public boolean isLeaf()
	{
		return children == null;
	}
	
	@Override
	public Field getJump()
	{
		if(jump == null && parent != null)
			return parent.getJump(); //return jump of parent
		else
			return jump; //return own jump (possibly null)
	}
	
	@Override
	public boolean isNoColumn()
	{
		return root.noColumn; //!!!
	}
	
	@Override
	public boolean isOptional()
	{
		return root.optional;
	}

	@Override
	public List<File> getFiles(FileStorageProvider fileStorageProvider)
	{
		List<File> paths = new ArrayList<File>();
		if(hasImage())
			CollectionUtils.addIgnoreNull(paths, form.getProject().getImageFile(fileStorageProvider, imageRelativePath));
		for(ChoiceField child : getChildren())
			CollectionUtils.addAllIgnoreNull(paths, child.getFiles(fileStorageProvider));
		return paths;
	}
	
	public String toString()
	{
		return toString(false);
	}
	
	public String toString(boolean printValue)
	{
		if(printValue)
			return "ChoiceField " + id + (value != null ? " (value: " + value + ")" : " (no value set)");
		else
			return id;
	}
	
	@Override
	protected IntegerColumn createColumn(String name)
	{
		if(!isRoot())
			throw new IllegalStateException("createColumn() should only be called on a root ChoiceField object.");
		dictionary.initialise(this); //!!!
		if(dictionary.isEmpty())
		{	//no values set
			form.addWarning("noColumn was forced to true on ChoiceField " + getID() + " because it has no values.");
			noColumn = true; //!!!
			return null;
		}
		else
		{	
			boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
			
			//Create column:
			IntegerColumn col = new IntegerColumn(name, colOptional, 0, dictionary.size() - 1);
			
			// Add virtual columns to it:
			//	Value String column:
			StringListMapper itemValueMapper = new StringListMapper(dictionary.serialise(new DictionarySerialiser<ChoiceField>()
			{
				@Override
				public String serialise(ChoiceField item)
				{
					return item.value;
				}
			}));
			col.addVirtualVersion(StringColumn.ForCharacterCount(VALUE_VIRTUAL_COLOMN_TARGET_NAME, colOptional, Math.max(itemValueMapper.getMaxStringLength(), 1)), itemValueMapper);
			//	Image path column:
			StringListMapper itemImgMapper = new StringListMapper(dictionary.serialise(new DictionarySerialiser<ChoiceField>()
			{
				@Override
				public String serialise(ChoiceField item)
				{
					return item.imageRelativePath;
				}
			}));
			col.addVirtualVersion(StringColumn.ForCharacterCount(IMAGE_VIRTUAL_COLOMN_TARGET_NAME, colOptional, Math.max(itemImgMapper.getMaxStringLength(), 1)), itemImgMapper);
			
			// Return the column:
			return col;
		}
	}
	
	/**
	 * @return the value (possibly null)
	 */
	public String getValue()
	{
		return value;
	}
	
	public ChoiceDictionary getDictionary()
	{
		return dictionary;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterChoiceField(this, arguments, withPage);
	}
	
	@Override
	public <V, UI extends CollectorUI<V, UI>> ChoiceUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createChoiceUI(this);
	}
	
	@Override
	public List<String> getDocExtras()
	{
		return Arrays.asList(this.imageRelativePath, this.id);
	}
	
	@Override
	public IntegerColumn getColumn()
	{
		// Non-root:
		if(!isRoot())
			return root.getColumn();
		// Root:
		return (IntegerColumn) super.getColumn();
	}
	
	/**
	 * Returns the selected choice for the given ChoiceField 
	 * 
	 * @return the selected choice
	 */
	public ChoiceField getSelectedChoice(Record record)
	{
		if(record == null || isNoColumn())
			return null;
		Long choiceIdx = getColumn().retrieveValue(record);
		if(choiceIdx != null)
			return getDictionary().lookupItem(choiceIdx.intValue());
		else
			return null;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof ChoiceField)
		{
			ChoiceField that = (ChoiceField) obj;
			return	super.equals(that) && // Field#equals(Object)
					(this.parent != null ? that.parent != null && this.parent.getID().equals(that.parent.getID()) : that.parent == null) &&
					(this.root != null ? that.root != null && this.root.getID().equals(that.root.getID()) : that.root == null) &&
					this.getChildren().equals(that.getChildren()) &&
					(this.imageRelativePath != null ? this.imageRelativePath.equals(that.imageRelativePath) : that.imageRelativePath == null) &&
					(this.answerDesc != null ? that.answerDesc.equals(that.answerDesc) : that.answerDesc == null) &&
					(this.questionDesc != null ? that.questionDesc.equals(that.questionDesc) : that.questionDesc == null) &&
					this.cols == that.cols &&
					this.rows == that.rows &&
					(this.altText != null ? this.altText.equals(that.altText) : that.altText == null) &&
					this.crossed == that.crossed &&
					this.crossColor.equals(that.crossColor) &&
					(this.value != null ? this.value.equals(that.value) : that.value == null);
					// Do not include dictionary at all here! It is unnecessary and causes an endless loop!
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		hash = 31 * hash + (parent != null ? parent.getID().hashCode() : 0);
		hash = 31 * hash + (root != null ? root.getID().hashCode() : 0);
		hash = 31 * hash + getChildren().hashCode();
		hash = 31 * hash + (imageRelativePath != null ? imageRelativePath.hashCode() : 0);
		hash = 31 * hash + (answerDesc != null ? answerDesc.hashCode() : 0);
		hash = 31 * hash + (questionDesc != null ? questionDesc.hashCode() : 0);
		hash = 31 * hash + cols;
		hash = 31 * hash + rows;
		hash = 31 * hash + (altText != null ? altText.hashCode() : 0);
		hash = 31 * hash + (crossed ? 0 : 1);
		hash = 31 * hash + crossColor.hashCode();
		hash = 31 * hash + (value != null ? value.hashCode() : 0);
		// Do not include dictionary at all here! It is unnecessary and causes an endless loop!
		return hash;
	}
	
	/**
	 * A Dictionary for ChoiceFields.
	 * 
	 * Holds a (Hash)Map (itemToIndex) which maps "valued" (i.e. with non-null value String) leaves to indexes,
	 * which are used to store the value (i.e. the choice made) of the ChoiceField tree.
	 * 
	 * Also holds an (Array)List which allows choices to be looked up by index.
	 *
	 * @author mstevens
	 */
	public static class ChoiceDictionary extends Dictionary<ChoiceField>
	{

		/**
		 * <b>Note:</b> This method should only be called after the whole choice tree is parsed & constructed (i.e. from createColumn()).
		 */
		protected void initialise(ChoiceField root)
		{
			if(root.isRoot())
				traverse(root);
			else
				throw new IllegalArgumentException("ChoiceDictionary can only be initialised from the root choice.");
		}
	
		/**
		 * Recursive method which implements a depth-first traversal that finds all leaves and stores them in the dictionary provided they carry a value.
		 */
		private void traverse(ChoiceField choice)
		{
			if(choice.isLeaf())
			{
				if(choice.getValue() != null) // (do not merge the if's)
				{
					itemToIndex.put(choice, indexed.size());
					indexed.add(choice);
				}
			}
			else
			{
				for(ChoiceField child : choice.children) // Depth-first traversal
					traverse(child); // recursive call
			}
		}
		
		@Override
		protected List<String> getDocHeaders()
		{
			List<String> hdrs = super.getDocHeaders();
			hdrs.add("IMG");
			hdrs.add("ID/PATH");
			return hdrs;
		}
				
	}
	
}
