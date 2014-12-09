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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.Mode;
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
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;

/**
 * Helper class used to determine the optionality of columns backing Fields of a Form.
 * 
 * @author mstevens
 */
public class ColumnOptionalityAdvisor
{

	// STATICS-------------------------------------------------------
	static public ColumnOptionalityAdvisor For(Form form)
	{
		Analyser analyser = new Analyser(form);
		
		// Start analysis for CREATE mode:
		analyser.traverse(Mode.CREATE);
		// (analysing for EDIT mode should not be necessary)
		
		// Return advisor:
		return new ColumnOptionalityAdvisor(form, analyser.getByPassableOptionalFieldsWithColumn());
	}
	
	// DYNAMICS------------------------------------------------------
	private final Form form;
	private final Set<Field> byPassableNonOptionalFieldsWithColumn;
	
	/**
	 * @param form
	 * @param byPassableNonOptionalFieldsWithColumn
	 */
	private ColumnOptionalityAdvisor(Form form, Set<Field> byPassableNonOptionalFieldsWithColumn)
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
	public Set<Field> getByPassableNonOptionalFieldsWithColumn()
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
	private static class Analyser implements FieldVisitor
	{
		
		private Form form;
		private Mode mode;
		private List<Field> fieldsToCheck;
		private Field currentField;
		private Stack<PassedField> passedFields;
		private Stack<List<FieldWithArguments>> nextFields;
		private Set<Field> bypassableFields;
		
		public Analyser(Form form)
		{
			this.form = form;
			fieldsToCheck = assembleFieldsToCheck(form.getFields(), new ArrayList<Field>());	
		}
		
		/**
		 * Finds all top-level and Page-owned fields which have a column and are non-optional 
		 * 
		 * @param fields
		 * @param result
		 * @return
		 */
		private List<Field> assembleFieldsToCheck(List<Field> fields, List<Field> result)
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
		
		public void traverse(Mode mode)
		{
			this.mode = mode;
			this.currentField = null;
			this.passedFields = new Stack<PassedField>();
			this.nextFields = new Stack<List<FieldWithArguments>>();
			
			// Start at the beginning:
			List<FieldWithArguments> startList = nextFields.push(new ArrayList<FieldWithArguments>());
			startList.add(new FieldWithArguments(form.getStartField()));
			
			// Depth-first traversal loop:
			while(!nextFields.isEmpty())
			{
				if(!nextFields.peek().isEmpty())
					goTo(nextFields.peek().remove(0));
				else
				{	// Backtrack...
					nextFields.pop();
					if(!passedFields.isEmpty()) // there is always 1 item less on the visited stacks
						passedFields.pop();
				}
			}
		}
		
		public void goTo(FieldWithArguments nextFieldAndArguments)
		{		
			// Null check...
			if(nextFieldAndArguments == null || nextFieldAndArguments.field == null)
				return;
		
			// Avoid endless loops:
			for(PassedField ps : passedFields)
				if(ps.field == nextFieldAndArguments.field)
					return;
			
			// the nextField the "current field":
			currentField = nextFieldAndArguments.field;
			nextFields.push(new ArrayList<FieldWithArguments>());
			
			if(!Controller.IsFieldToBeShown(mode, currentField))
			{	// Skip the next field if it is not meant to be shown in the current form mode:
				passedFields.push(PassedField.Skipped(currentField));
				goForward();
			}
			else
			{	// actually visit next field:
				passedFields.push(PassedField.Visited(currentField));
				currentField.enter(this, nextFieldAndArguments.arguments, false); // there's no UI so return value is ignored
			}
		}
		
		private void goForward()
		{	// add next field to nextFields stack/list:
			nextFields.peek().add(form.getNextFieldAndArguments(currentField));
		}
		
		private boolean enterLinearField(boolean withPage)
		{
			if(!withPage)
			{
				// simulate form triggers triggering while on current field:
				simulateTriggers(form.getTriggers());
				
				// next:
				goForward();
			}
			return false;
		}
		
		public void simulateTriggers(List<Trigger> triggers)
		{
			for(Trigger trigger : triggers)
				if(trigger.getJump() != null)
					nextFields.peek().add(new FieldWithArguments(trigger.getJump(), trigger.getNextFieldArguments()));
		}

		@Override
		public boolean enterChoiceField(ChoiceField cf, FieldParameters arguments, boolean withPage)
		{
			if(withPage || cf.isLeaf())
				return true; // should never happen
			// We are not on a page and this is not a leaf, so this choice will be displayed to the user
			
			// Children the user can choose from:
			boolean atLeast1Child = false;
			for(ChoiceField child : cf.getChildren())
				if(Controller.IsFieldEnabled(mode, child))
				{
					atLeast1Child = true;
					// add to next stack/list:
					if(child.isLeaf())
						// Go to next/jump of chosenChild (not to the chosen child itself because it is a leaf):
						nextFields.peek().add(form.getNextFieldAndArguments(child));
					else
						// Go to chosen child:
						nextFields.peek().add(new FieldWithArguments(child, cf.getNextFieldArguments()));
				}
			
			// Simulate form triggers firing before user is able to make a choice:
			if(atLeast1Child)
				simulateTriggers(form.getTriggers());
			
			//	Next field of choice itself:
			//		either because there are no children (meaning we automatically advance),
			//		or because the user hits "forward" instead of making a choice (if field is optional and the forward button is shown)
			if(!atLeast1Child || (cf.isOptional() && cf.isControlAllowedToBeShown(Control.FORWARD, mode)))
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
			if(page.isControlAllowedToBeShown(Control.FORWARD, mode))
				goForward();
			
			// Enter child fields (but signal that they are entered as part of entering the page):
			for(Field f : page.getFields())
			{	
				if(Controller.IsFieldToBeShown(mode, f))
				{
					passedFields.peek().addSubField(f);
					f.enter(this, FieldParameters.EMPTY, true); // enter with page (but don't pass on the arguments)
				}
			}
			
			return false;
		}
		
		@Override
		public boolean enterButtonField(ButtonField buttonField, FieldParameters arguments, boolean withPage)
		{	
			// Simulate jump upon click:
			if(buttonField.getJump() != null)
				nextFields.peek().add(new FieldWithArguments(buttonField.getJump(), buttonField.getNextFieldArguments()));
			
			if(!withPage)
			{
				// "Forward" press (if allowed):
				if(buttonField.isOptional() && buttonField.isControlAllowedToBeShown(Control.FORWARD, mode))
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
				// Assemble list of all visited fields:
				List<Field> allVisited = new ArrayList<Field>();
				for(PassedField ps : passedFields)
					if(!ps.skipped)
					{
						allVisited.add(ps.field);
						allVisited.addAll(ps.getSubFields());
					}
				
				// Which of the fieldsToCheck have *not* been visited?
				for(Field field : fieldsToCheck)
				{
					if(	// avoid checking fields which we already know are "by-passable":
						(bypassableFields == null || !bypassableFields.contains(field)) &&
						// check if field was *not* visited in this traversal, ...
						!allVisited.contains(field))
					{	// ... field was *not* visited while traversing from start to end/saving: so it is "by-passable"
						if(bypassableFields == null)
							bypassableFields = new HashSet<Field>();
						bypassableFields.add(field);
						// Debug:
						/*System.out.println("\nBypassable: " + field.getID());
						System.out.print("Passed: ");
						for(Field f : visitedFields)
							System.out.print(f.getID() + ",");
						System.out.println("");*/
					}
				}
			}
			// We go nowhere from here, backtrack will start...
			
			return false;
		}
		
		public Set<Field> getByPassableOptionalFieldsWithColumn()
		{
			return bypassableFields == null ? Collections.<Field> emptySet() : bypassableFields;
		}
		
		/**
		 * Helper class
		 * 
		 * @author mstevens
		 */
		static private class PassedField
		{
			
			// STATICS-------------------------------------
			static public PassedField Visited(Field field)
			{
				return new PassedField(field, false);
			}
			
			static public PassedField Skipped(Field field)
			{
				return new PassedField(field, true);
			}
			
			// DYNAMICS------------------------------------
			final Field field;
			final boolean skipped;
			List<Field> subFields;
			
			/**
			 * @param field
			 * @param skipped
			 * @param subfields
			 */
			private PassedField(Field field, boolean skipped)
			{
				this.field = field;
				this.skipped = skipped;
			}
			
			public void addSubField(Field subField)
			{
				if(skipped)
					throw new IllegalStateException("A skipped field cannot have subfields");
				if(subFields == null)
					subFields = new ArrayList<Field>();
				subFields.add(subField);
			}
			
			public List<Field> getSubFields()
			{
				return subFields == null ? Collections.<Field> emptyList() : subFields;
			}
			
		}

	}

}
