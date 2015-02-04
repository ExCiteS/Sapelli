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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Trigger;
import uk.ac.ucl.excites.sapelli.collector.model.fields.BelongsToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.EndField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LinksToField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.shared.collections.EmptyQueue;
import uk.ac.ucl.excites.sapelli.shared.collections.SingletonQueue;

/**
 * Helper class used to determine the optionality of columns backing Fields of a Form.
 * 
 * @author mstevens
 */
public final class ColumnOptionalityAdvisor
{

	// STATICS-------------------------------------------------------
	static public ColumnOptionalityAdvisor For(final Form form)
	{
		final Analyser analyser = new Analyser(form);
		
		// Start analysis for CREATE mode:
		analyser.traverse(Mode.CREATE);
		// (analysing for EDIT mode should not be necessary)
		
		// Return advisor:
		return new ColumnOptionalityAdvisor(form, analyser.bypassableFields);
	}
	
	private static final Queue<FieldWithArguments> EMPTY_QUEUE = new EmptyQueue<FieldWithArguments>();
	
	// DYNAMICS------------------------------------------------------
	private final Form form;
	private final List<Field> byPassableNonOptionalFieldsWithColumn;
	
	/**
	 * @param form
	 * @param byPassableNonOptionalFieldsWithColumn
	 */
	private ColumnOptionalityAdvisor(Form form, List<Field> byPassableNonOptionalFieldsWithColumn)
	{
		this.form = form;
		this.byPassableNonOptionalFieldsWithColumn = byPassableNonOptionalFieldsWithColumn;
	}

	/**
	 * Determines the optionality for the column that will back a given field.
	 * The applied rule is as follows:
	 * 	- optional fields get an optional column;
	 * 	- non-optional fields get a non-optional column provided they *cannot* be by-passed, if they *can* be by-passed they get an optional column
	 * 
	 * For historic reference:
	 * 	The old way to determine column optionality (before introduction of by-passable field detection) was:
	 * 		columnOptionality = (field.optional != Optionalness.NEVER);
	 * 	I.e. fields which optionality ALWAYS or NOT_IF_REACHED got an optional column, fields with optionality NEVER got a non-optional column. 
	 * 
	 * @param fieldWithColumn the field for which to determine column optionality
	 * @return the optionality value (true = optional, false = mandatory/non-optional) for the column that will back the given field
	 */
	public boolean getColumnOptionality(Field fieldWithColumn)
	{
		if(fieldWithColumn == null)
			throw new NullPointerException("Field cannot be null!");
		if(fieldWithColumn.form != form)
			throw new IllegalArgumentException("This field does not belong to the correct form.");
		if(fieldWithColumn.isNoColumn())
			throw new IllegalArgumentException("This is a noColumn field!");
		if(fieldWithColumn.isOptional())
			return true;
		else
			return byPassableNonOptionalFieldsWithColumn.contains(fieldWithColumn);
	}
	
	/**
	 * @return
	 */
	public List<Field> getByPassableNonOptionalFieldsWithColumn()
	{
		return byPassableNonOptionalFieldsWithColumn;
	}

	/**
	 * Simulates all possible traversals of a form in order to find non-optional, column-holding
	 * fields (either top-level or Page-owned) which can be by-passed. A field can be by-passed
	 * if it is possible to traverse the form (in CREATE mode) from the start field to a saving
	 * EndField without visiting (i.e. entering) the field in question.
	 * 
	 * @author mstevens
	 */
	private final static class Analyser implements FieldVisitor
	{
		
		private final Form form;
		private final List<Field> fieldsToCheck;
		private final List<Field> bypassableFields;
		
		private Mode mode;
		
		/**
		 * Contains visited Fields and nulls for skipped fields
		 */
		private final Stack<Field> passed;
		
		/**
		 * Contains skipped Fields
		 */
		private final Stack<Field> skipped;
		
		private final Set<FieldWithArguments> assembleNextFields;
		
		private Field currentField;
		
		public Analyser(final Form form)
		{
			this.form = form;
			this.fieldsToCheck = new LinkedList<Field>(assembleFieldsToCheck(form.getFields(), new HashSet<Field>()));
			this.bypassableFields = new ArrayList<Field>(fieldsToCheck.size());
			this.passed = new Stack<Field>();
			this.skipped = new Stack<Field>();
			this.assembleNextFields = new HashSet<FieldWithArguments>(8); // initial capacity of 8 (rather than the default of 16)
		}
		
		/**
		 * Finds all top-level and Page-owned fields which have a column and are non-optional 
		 * 
		 * @param fields
		 * @param result
		 * @return
		 */
		private Set<Field> assembleFieldsToCheck(final List<Field> fields, final Set<Field> result)
		{
			for(Field f : fields)
			{
				if(f instanceof Page)
					assembleFieldsToCheck(((Page) f).getFields(), result);
				else if(!f.isNoColumn() && !f.isOptional())
					result.add(f);
			}
			return result;
		}
		
		public void traverse(final Mode mode)
		{
			this.mode = mode;
			
			// Fields that are not to be shown in the current mode are will be "by-passed" by definition...
			Iterator<Field> iter = fieldsToCheck.iterator();
			while(iter.hasNext())
			{
				Field fieldToCheck = iter.next();
				if(!Controller.IsFieldToBeShown(mode, fieldToCheck))
				{
					bypassableFields.add(fieldToCheck);
					iter.remove(); // !!! remove field from fieldsToCheck (we now know it is "by-passable" so we don't need to check it against paths)
				}
			}
			if(fieldsToCheck.isEmpty())
				return;
			
			// Do a depth-first traversal to investigate all possible paths through the form to find which of the fields to check are "by-passable"...
			//	Initialise...
			this.currentField = null;
			this.passed.clear();
			this.skipped.clear();
			//	DFT stack (containing lists of fields-with-args left to visit):
			final Stack<Queue<FieldWithArguments>> toVisit = new Stack<Queue<FieldWithArguments>>();
			//	Start at the beginning:
			toVisit.push(new SingletonQueue<FieldWithArguments>(new FieldWithArguments(form.getStartField())));
			//	DFT loop:
			do
			{
				Queue<FieldWithArguments> queue = toVisit.peek();
				if(!queue.isEmpty())
				{
					Queue<FieldWithArguments> nextFields = goTo(queue.poll());
					if(nextFields != null)
						toVisit.push(nextFields); // Note: nextFields may be empty (but will never contain nulls)
				}
				else
				{	// Backtrack...
					toVisit.pop();
					while(!passed.isEmpty()) // there is always 1 item less on the passed stack
					{
						Field popped = passed.pop();
						if(popped == null)
							skipped.pop(); // a null on the passed stack corresponds to a skipped field on the skipped stack
						else if(popped.isOnPage())
							continue; // if the field is part of a page we must keep popping until we pop the containing page
						break;
					}
				}
			}
			while(!toVisit.isEmpty() && !fieldsToCheck.isEmpty());
		}
		
		public Queue<FieldWithArguments> goTo(final FieldWithArguments nextFieldAndArguments)
		{
			// Avoid endless loops:
			if(passed.contains(nextFieldAndArguments.field) || skipped.contains(nextFieldAndArguments.field))
				return null;
			
			// The nextField becomes the "current field":
			currentField = nextFieldAndArguments.field;
			
			// Clear assembleNextFields set:
			assembleNextFields.clear();
			
			if(Controller.IsFieldToBeShown(mode, currentField))
			{	
				// This field would be shown to the user...
				passed.push(currentField); // remember we visited it

				// Actually enter the field:
				currentField.enter(this, nextFieldAndArguments.arguments, false); // there's no UI so return value is ignored
			}
			else
			{
				// This field is not meant to be shown in the current form mode, so skip it...
				skipped.push(currentField); // remember we skipped the field (for loop check)
				passed.push(null); // *and* also insert a null on passed skip
				
				// Go to field below:
				addNext(form.getNextFieldAndArguments(currentField, false)); // no jump allowed
			}
			
			// Return queue of next fields (w/ args) to visit:
			return assembleNextFields.isEmpty() ? EMPTY_QUEUE : new ArrayDeque<FieldWithArguments>(assembleNextFields);
		}
		
		private void addNext(final FieldWithArguments next)
		{
			if(next != null)
				assembleNextFields.add(next);
		}
		
		private void goForward()
		{
			addNext(form.getNextFieldAndArguments(currentField, true)); // jump allowed
		}
		
		private boolean enterLinearField(final boolean withPage)
		{
			if(!withPage)
			{
				// Simulate form triggers triggering while on current field:
				simulateTriggers(form.getTriggers());
				
				// Next:
				goForward();
			}
			return false;
		}
		
		public void simulateTriggers(final List<Trigger> triggers)
		{
			for(Trigger trigger : triggers)
				if(trigger.getJump() != null)
					addNext(new FieldWithArguments(trigger.getJump(), trigger.getNextFieldArguments()));
		}

		@Override
		public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments, boolean withPage)
		{
			if(withPage)
				return false; // should never happen
			
			boolean atLeast1Child = false;
			if(!cf.isLeaf())
				// Children the user can choose from:
				for(ChoiceField child : cf.getChildren())
					if(Controller.IsFieldEnabled(mode, child))
					{
						atLeast1Child = true;
						// Add to next stack/list:
						addNext(new FieldWithArguments(child, cf.getNextFieldArguments())); // Note: we also add children which are leaves because we want them in the path
					}
			
			// Simulate form triggers firing before user is able to make a choice:
			if(atLeast1Child)
				simulateTriggers(form.getTriggers());
			
			//	Advance to next field of choice itself, either because...
			//		this is a leaf,
			//		there are no enabled children (meaning we automatically advance),
			//		or because the user hits "forward" instead of making a choice (if field is optional and the forward button is shown)
			if(!atLeast1Child || (cf.isOptional() && cf.isControlAllowedToBeShown(Control.Type.Forward, mode)))
				goForward();
			
			return false;
		}
		
		@Override
		public boolean enterPage(Page page, FieldParameters arguments)
		{			
			// Simulate page triggers
			simulateTriggers(page.getTriggers());
			
			// Simulate form triggers
			simulateTriggers(form.getTriggers());
			
			// Simulate forward press from page (if allowed):
			if(page.isControlAllowedToBeShown(Control.Type.Forward, mode))
				goForward();
			
			// Enter child fields (but signal that they are entered as part of entering the page):
			for(Field fieldOnPage : page.getFields())
				if(Controller.IsFieldToBeShown(mode, fieldOnPage))
				{
					// Remember the field is visited:
					passed.push(fieldOnPage);
					// Enter it:
					fieldOnPage.enter(this, FieldParameters.EMPTY, true); // enter with page (but don't pass on the arguments)
				}
			
			return false;
		}
		
		@Override
		public boolean enterButtonField(ButtonField buttonField, FieldParameters arguments, boolean withPage)
		{	
			// Simulate jump upon click:
			if(buttonField.getJump() != null)
				addNext(new FieldWithArguments(buttonField.getJump(), buttonField.getNextFieldArguments()));
			
			if(!withPage)
			{
				// "Forward" press (if allowed):
				if(buttonField.isOptional() && buttonField.isControlAllowedToBeShown(Control.Type.Forward, mode))
					goForward();
				
				// Simulate form triggers firing before user is able to click button or hit "forward"
				simulateTriggers(form.getTriggers());
			}
			
			return false;
		}

		@Override
		public boolean enterMediaField(MediaField mf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterLocationField(LocationField lf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterOrientationField(OrientationField of, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}
		
		@Override
		public boolean enterLinksTo(LinksToField linksTo, FieldParameters arguments)
		{
			return enterLinearField(false); // TODO change when implemented in Controller
		}

		@Override
		public boolean enterBelongsTo(BelongsToField belongsTo, FieldParameters arguments)
		{
			return enterLinearField(false);
		}

		@Override
		public boolean enterTextBoxField(TextBoxField tbf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterCheckboxField(CheckBoxField cbf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterLabelField(LabelField lblf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterMultiListField(MultiListField mlf, FieldParameters arguments, boolean withPage)
		{
			return enterLinearField(withPage);
		}

		@Override
		public boolean enterEndField(EndField ef, FieldParameters arguments)
		{
			if(ef.isSave())
			{
				// Which of the fieldsToCheck have *not* been visited?
				Iterator<Field> iter = fieldsToCheck.iterator();
				while(iter.hasNext())
				{
					final Field fieldToCheck = iter.next();
					if(!passed.contains(fieldToCheck))
					{	// ... field would have been shown in the current mode but was *not* visited while traversing from start to end/saving: so it is "by-passable"
						bypassableFields.add(fieldToCheck);
						iter.remove(); // !!! remove field from fieldsToCheck (we now know it is "by-passable" so we don't need to check it against other paths)
					}
				}
			}
			// We go nowhere from here, backtrack will start...
			return false;
		}

	}

}
