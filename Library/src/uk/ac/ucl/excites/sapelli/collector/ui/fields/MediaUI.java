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

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens, benelliott
 *
 * @param <MF>
 * @param <V>
 */
public abstract class MediaUI<MF extends MediaField, V, UI extends CollectorUI<V, UI>> extends SelfLeavingFieldUI<MF, V, UI>
{
	
	// STATIC -----------------------------------------------------------------
	/**
	 * The different modes (or "screens") the MediaUI can be in when being shown on its own (i.e not on a Page).
	 */
	protected static enum Mode
	{
		CAPTURE,
		CAPTURE_FROM_GALLERY,
		GALLERY,
		/**
		 * Review a single item after its capture
		 */
		REVIEW_ITEM_POST_CAPTURE,
		/**
		 * Showing a single item from gallery
		 */
		REVIEW_ITEM_FROM_GALLERY,
		/**
		 * Alternative to gallery mode when field.max = 1
		 */
		REVIEW_ITEM_PSEUDO_GALLERY
	}
	
	// Keys to use when obtaining values from field arguments:
	protected static final String REVIEW_FROM_GALLERY_OFFSET_KEY = "REVIEW_FROM_GALLERY_OFFSET_KEY";
	protected static final String GO_TO_POST_CAPTURE_REVIEW = "GO_TO_POST_CAPTURE_REVIEW";
	protected static final String GO_TO_CAPTURE_KEY = "GO_TO_CAPTURE";
	
	// DYNAMIC ----------------------------------------------------------------
	private Mode mode = null;
	private MediaFile fileToReview = null;
		
	public MediaUI(MF field, CollectorController<UI> controller, UI collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	@Override
	protected void update(Record record, FieldParameters fieldArgs)
	{
		// Clear previous state:
		fileToReview = null;
		mode = null;
		
		if(!isFieldShownAlone())
			return;
		
		if(!field.hasAttachements(record))
		{	// we have no attachments yet --> go to capture
			mode = Mode.CAPTURE;
		}
		// Changing mode without leaving the MediaField:
		else if(fieldArgs.getBoolean(GO_TO_CAPTURE_KEY, false))
		{	// we have been explicitly told to go to capture (by means of the "add more" button in the gallery):
			mode = Mode.CAPTURE_FROM_GALLERY;
			fieldArgs.remove(GO_TO_CAPTURE_KEY); // avoid re-entry
		}
		else if(fieldArgs.getBoolean(GO_TO_POST_CAPTURE_REVIEW, false))
		{
			if(field.getMax() == 1)
			{
				// Review last item:
				mode = Mode.REVIEW_ITEM_POST_CAPTURE;
				fileToReview = field.getLastAttachment(controller.getFileStorageProvider(), record);
			}
			else
				// Review using gallery
				mode = Mode.GALLERY;
			fieldArgs.remove(GO_TO_POST_CAPTURE_REVIEW); // avoid re-entry
		}
		else if(fieldArgs.getValue(REVIEW_FROM_GALLERY_OFFSET_KEY) != null)
		{	// Review specific item:
			mode = Mode.REVIEW_ITEM_FROM_GALLERY;
			fileToReview = field.getAttachmentFromOffset(controller.getFileStorageProvider(), record, Long.parseLong(fieldArgs.remove(REVIEW_FROM_GALLERY_OFFSET_KEY))); // use & remove (to avoid re-entry) path argument
		}
		// When re-entering MediaField after having left it (e.g. after back press from next field):
		else if(field.getMax() == 1)
		{	// max is 1 and we have at least 1 attachment --> go to single item review (instead of gallery)
			mode = Mode.REVIEW_ITEM_PSEUDO_GALLERY;
			fileToReview = field.getLastAttachment(controller.getFileStorageProvider(), record);
		}
		else
		{	// we have at least 1 attachment, the max is > 1 --> go to gallery
			mode = Mode.GALLERY;
		}
	}
	
	/**
	 * @return the current {@link MediaUI.Mode}
	 */
	public Mode getMode()
	{
		return mode;
	}
	
	protected boolean isInCaptureMode()
	{
		return mode == Mode.CAPTURE || mode == Mode.CAPTURE_FROM_GALLERY;
	}
	
	protected boolean isInReviewItemMode(boolean exceptPseudoGallery)
	{
		return
			mode == Mode.REVIEW_ITEM_POST_CAPTURE ||
			mode == Mode.REVIEW_ITEM_FROM_GALLERY ||
			(!exceptPseudoGallery && mode == Mode.REVIEW_ITEM_PSEUDO_GALLERY);
	}
	
	/**
	 * @return the fileToReview
	 */
	public MediaFile getFileToReview()
	{
		return fileToReview;
	}

	@Override
	protected abstract void cancel(); // force concrete subclass to implement this (e.g. to stop audio recording)!
	
	/**
	 * Logs the attachment of a media file, and requests that the controller proceed to the appropriate field.
	 * 
	 * @param mediaAttachment - the file to be attached to {@code field} in the current record.
	 */
	public void attachMedia(MediaFile mediaAttachment)
	{
		if(mediaAttachment != null && mediaAttachment.file.exists())
		{
			controller.addLogLine("ATTACHMENT", field.id, mediaAttachment.file.getAbsolutePath());
			// add it to the record
			field.addAttachmentToRecord(mediaAttachment, controller.getCurrentRecord());
			// mark it to be added when the user saves their session
			controller.addAttachment(mediaAttachment);
		}
		else
		{
			controller.addLogLine("ATTACHMENT", field.id, "-NONE-");
		}
	}
	
	/**
	 * Logs the deletion of a media file, removes it from the record and cancels the request of it being saved at the end
	 * of the user's session.
	 * 
	 * @param mediaAttachment - the attachment to remove
	 */
	public void removeMedia(MediaFile mediaAttachment)
	{
		controller.addLogLine("ATTACHMENT REMOVED", field.getID(), mediaAttachment.file.getName());
		field.removeAttachmentFromRecord(mediaAttachment, controller.getCurrentRecord());
		
		controller.discardAttachment(mediaAttachment);
	}
	
	protected boolean showCreateButton()
	{
		return !field.isMaxAttachmentsReached(controller.getCurrentRecord());
	}
	
	@Override
	public boolean isValid(Record record)
	{
		return field.isNoColumn() || (field.getAttachmentCount(record) >= field.getMin() && field.getAttachmentCount(record) <= field.getMax());
	}

}
