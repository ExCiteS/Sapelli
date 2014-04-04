/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
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
		if(!controller.isFieldEndabled(chosenChild))
			return;
		// Store value if chosenChild is a leaf and the field has a column:
		if(chosenChild.isLeaf() && !field.isNoColumn())
			field.getColumn().storeValue(controller.getCurrentRecord(), field.getDictionary().lookupIndex(chosenChild));
		// Go to chosenChild:
		controller.goTo(chosenChild, !chosenChild.isLeaf());
		
		/* Note 1:	chosenChild becomes the new currentField (i.e. we go one level "down" in the choice tree),
		 * 			but if it is a leaf the controller will call goForward() from enterChoiceField().
		 * 
		 * Note 2:	if the chosenChild is not a leaf we "force" the goTo because otherwise validation would keep us from
		 * 			advancing. If it is a leaf we let validation happen.
		 */
	}

}
