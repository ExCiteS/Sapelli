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
	
	/**
	 * Logs the attachment of a media file, and requests that the controller proceed to the appropriate field.
	 * 
	 * @param mediaAttachment - the file to be attached to {@code field} in the current record.
	 * @param userRequested
	 * @param goForward - whether to go forward to the next field or re-enter the current field with the new attachment (important
	 * if multiple attachments can be added to the same field).
	 */
	public void attachMedia(File mediaAttachment)
	{
		if(mediaAttachment != null && mediaAttachment.exists())
		{
			// log the attachment
			controller.addLogLine("ATTACHMENT", field.getID(), mediaAttachment.getName());
			// add it to the record
			field.addAttachmentToRecord(mediaAttachment, controller.getCurrentRecord());
			// mark it to be added when the user saves their session
			controller.addAttachment(mediaAttachment);
			// goto next/jump field
		}
		else
		{
			// log empty attachment
			controller.addLogLine("ATTACHMENT", field.getID(), "-NONE-");
		}
	}
	
	/**
	 * Logs the deletion of a media file, removes it from the record and cancels the request of it being saved at the end
	 * of the user's session.
	 * @param mediaAttachment - the attachment to remove
	 */
	public void removeMedia(File mediaAttachment)
	{
			controller.addLogLine("ATTACHMENT REMOVED", field.getID(), mediaAttachment.getName());
			field.removeAttachmentFromRecord(mediaAttachment, controller.getCurrentRecord());
			controller.discardAttachment(mediaAttachment);
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
