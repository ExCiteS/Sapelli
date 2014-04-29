/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;

/**
 * @author mstevens
 *
 */
public abstract class ChoiceUI<V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<ChoiceField, V, UI>
{

	/**
	 * @param field
	 * @param controller
	 * @param collectorUI
	 */
	public ChoiceUI(ChoiceField choice, Controller controller, UI collectorUI)
	{
		super(choice, controller, collectorUI);
		if(choice.isLeaf()) // just in case...
			throw new IllegalArgumentException("Cannot display leaf choice.");
	}
	
	/**
	 * Note: chosenChild is not the current Field of the Controller, its current Field (also a ChoiceField) is its parent.
	 * 
	 * @param chosenChild
	 */
	protected void choiceMade(ChoiceField chosenChild)
	{
		if(!controller.isFieldEnabled(chosenChild))
			throw new IllegalArgumentException("This choice is disabled:" + chosenChild.getAltText()); // should never happen
		FieldWithArguments next;
		if(chosenChild.isLeaf())
		{	// Store value if the field has a column, the chosenChild is a leaf and it is known in the field dictionary (meaning it carries a value):
			if(!field.isNoColumn() && chosenChild.isLeaf() && field.getDictionary().contains(chosenChild))
				field.getColumn().storeValue(controller.getCurrentRecord(), field.getDictionary().lookupIndex(chosenChild));
			// Go to next/jump of chosenChild:
			next = field.getForm().getNextFieldAndArguments(chosenChild);
		}
		else
			// Go to chosen child:
			next = new FieldWithArguments(chosenChild, field.getNextFieldArguments()); // No arguments (i.e. FieldParameters) are passed from parent to child
		// Go...
		controller.goTo(next, !chosenChild.isLeaf() || !field.getDictionary().contains(chosenChild));
		/* Note 1:	chosenChild is not the currentField! The currentField (also a ChoiceField) is its parent.
		 * Note 2:	For leaves we cannot just call goForward() here because then we would first need to make
		 * 			the chosenChild the currentField, in which case it would end up in the fieldHistory which
		 * 			does not make sense because a leaf choice cannot be displayed on its own.
		 * Note 3:	If the chosenChild is not a leaf, or it is a "valueless" leaf, the goTo is "forced" because
		 * 			otherwise validation would keep us from advancing. If it is a "valued" leaf validation will
		 * 			happen. This means valueless leaves offer way out of choice trees, even non-optional ones.
		 * 			Form designers should use this with care (e.g. only using valueless leaves as "back jumps",
		 * 			and assuring the field will be revisited to acquire a value. */
	}

}
