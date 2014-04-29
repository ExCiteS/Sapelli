package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 * @param <MF>
 * @param <V>
 */
public abstract class MediaUI<MF extends MediaField, V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<MF, V, UI>
{

	public MediaUI(MF field, Controller controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}

	public void mediaDone(File mediaAttachment, boolean userRequested)
	{
		if(mediaAttachment != null && mediaAttachment.exists())
		{
			controller.addLogLine("ATTACHMENT", field.getID(), mediaAttachment.getName());
			
			field.incrementCount(controller.getCurrentRecord()); // Store/increase number of pictures/recordings taken
			
			// Store file:
			controller.addMediaAttachment(mediaAttachment);
			
			controller.goForward(userRequested); // goto next/jump field
		}
		else
		{
			controller.addLogLine("ATTACHMENT", field.getID(), "-NONE-");
			
			if(field.getOptional() != Optionalness.ALWAYS && field.getCount(controller.getCurrentRecord()) < 1)
				// at least one attachment is required & we have none:
				controller.goToCurrent(); // stay at this field ("return;" is not enough because if we are using a native app it needs to be restarted)
			else
				controller.goForward(userRequested); // goto next/jump field //TODO this needs changing when we allow review of photos/audio
		}
	}
	
	protected boolean showCreateButton()
	{
		return !field.isMaxReached(controller.getCurrentRecord());
	}
	
	@Override
	public boolean isValid(Record record)
	{
		return field.isNoColumn() || (field.getCount(record) >= field.getMin() && field.getCount(record) <= field.getMax());
	}
	
	@Override
	public abstract void cancel(); // force concrete subclass to implement this (e.g. to stop audio recording)!

}
