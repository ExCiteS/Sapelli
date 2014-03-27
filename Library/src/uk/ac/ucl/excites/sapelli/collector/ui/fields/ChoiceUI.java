/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;

/**
 * @author mstevens
 *
 */
public abstract class ChoiceUI<V> extends SelfLeavingFieldUI<ChoiceField, V>
{

	/**
	 * @param field
	 * @param controller
	 * @param collectorUI
	 */
	public ChoiceUI(ChoiceField choice, Controller controller, CollectorUI<V> collectorUI)
	{
		super(choice, controller, collectorUI);
		if(choice.isLeaf()) // just in case...
			throw new IllegalArgumentException("Cannot display leaf choice.");
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.FieldUI#isValid(uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord)
	 */
	@Override
	public boolean isValid(CollectorRecord record)
	{
		return false; // always invalid because we're not a at leaf!
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
		if(chosenChild.isLeaf())
		{
			// Store value
			if(!field.isNoColumn())
				((IntegerColumn) field.getRoot().getColumn()).storeValue(controller.getCurrentRecord(), field.getDictionary().lookupIndex(chosenChild));
			// Go to next field
			controller.goTo(controller.getCurrentForm().getNextField(chosenChild));
			/*
			 * We cannot use Controller#goForward() here because then we would first need to make the chosenChild the currentField, in which case it would end up in the
			 * fieldHistory which does not make sense because a leaf choice cannot be displayed on its own.
			 */
		}
		else
			controller.goTo(chosenChild); // chosenChild becomes the new currentField (we go one level down in the choice tree)
	}

}
