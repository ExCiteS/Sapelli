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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
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
			field.addAttachmentToRecord(mediaAttachment, controller.getCurrentRecord());
			controller.goForward(userRequested); // goto next/jump field
		}
		else
		{
			controller.addLogLine("ATTACHMENT", field.getID(), "-NONE-");
			if(!isValid(controller.getCurrentRecord()))
				// at least one attachment is required & we have none:
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE); // stay at this field ("return;" is not enough because if we are using a native app it needs to be restarted)
			else
				controller.goForward(userRequested); // goto next/jump field
		}
	}
	
	public void mediaAddedButNotDone(File mediaAttachment)
	{
		if(mediaAttachment != null && mediaAttachment.exists())
		{
			controller.addLogLine("ATTACHMENT", field.getID(), mediaAttachment.getName());
			field.addAttachmentToRecord(mediaAttachment, controller.getCurrentRecord());			
		}
		// do NOT go to next/jump field
		controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
	}
	
	public void removeMedia(File mediaAttachment)
	{
			controller.addLogLine("ATTACHMENT REMOVED", field.getID(), mediaAttachment.getName());
			field.removeAttachmentFromRecord(mediaAttachment, controller.getCurrentRecord());
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
	protected abstract void cancel(); // force concrete subclass to implement this (e.g. to stop audio recording)!

}
