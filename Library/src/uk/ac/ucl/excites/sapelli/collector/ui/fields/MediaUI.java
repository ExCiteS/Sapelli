package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field.Optionalness;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.SelfLeavingFieldUI;

/**
 * @author mstevens
 *
 * @param <MF>
 * @param <V>
 */
public abstract class MediaUI<MF extends MediaField, V> extends SelfLeavingFieldUI<MF, V>
{

	public MediaUI(MF field, Controller controller, CollectorUI<V> collectorUI)
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
			
			if(field.getOptional() != Optionalness.ALWAYS)
				// at least one attachment is required:
				controller.goTo(field); // stay at this field (TODO maybe a "return;" is enough here?)
			else
				controller.goForward(userRequested); // goto next/jump field //TODO this needs changing when we allow review of photos/audio
		}
	}
	
	protected boolean showCreateButton()
	{
		return !field.isMaxReached(controller.getCurrentRecord());
	}
	
	@Override
	public boolean isValid(CollectorRecord record)
	{
		return field.isNoColumn() || (field.getCount(record) >= field.getMin() && field.getCount(record) <= field.getMax());
	}
	
	@Override
	public abstract void cancel(); // force concrete subclass to implement this (e.g. to stop audio recording)!
	
}
