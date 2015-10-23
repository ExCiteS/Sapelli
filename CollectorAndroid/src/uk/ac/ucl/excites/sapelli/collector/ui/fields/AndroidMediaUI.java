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
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;

import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.LinearLayout;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ItemHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.util.android.ColourHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * An abstract class that represents a generic interface for capturing and reviewing media, with
 * the automatic usage of a "gallery" mode for review if the media field can have multiple attachments.
 * 
 * Subclasses must implement methods that are called when various control buttons are pressed, such as "onCapture" when
 * the media capture button (e.g. camera shutter) is tapped.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public abstract class AndroidMediaUI<MF extends MediaField> extends MediaUI<MF, View, CollectorView>
{
	
	// STATIC -------------------------------------------------------
	protected static final String TAG = "AndroidMediaUI";

	/**
	 * PAdding to put around the built-in play/stop/pause SVGs when displaying them in the review content
	 */
	protected static final float PLAYBACK_BUTTON_PADDING_DIP = 60.0f;
	
	// DYNAMIC ------------------------------------------------------
	private boolean mediaItemsChanged = false; 	// whether or not the media items have been changed (whether the gallery needs to be redrawn)
	private File captureFile; // file used to store media while it is being captured

	protected final boolean unblockUIAfterCaptureClick;
	
	protected final int fieldBackgroundColor;
	
	private CaptureView captureView;
	private ReviewView reviewView;
	private GalleryView galleryView;

	private OnPageView pageView;
	
	// global variable that holds the params for the capture/discard buttons
	private final LinearLayout.LayoutParams buttonParams; 

	/**
	 * @param field
	 * @param controller
	 * @param collectorUI
	 * @param unblockUIAfterCaptureClick indicates whether or not to immediately allow new user interactions (i.e. clicks) after the capture button is clicked
	 * after this method has returned.
	 */
	public AndroidMediaUI(MF field, CollectorController controller, CollectorView collectorUI, boolean unblockUIAfterCaptureClick)
	{
		super(field, controller, collectorUI);
		
		this.unblockUIAfterCaptureClick = unblockUIAfterCaptureClick;
		this.fieldBackgroundColor = ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR);
		
		// Initialise buttonParams:
		//	create button params to be used for all bottom-of-screen buttons:
		buttonParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, collectorUI.getControlHeightPx());
		//	add margin above button to separate it from content:
		buttonParams.setMargins(0, collectorUI.getSpacingPx(), 0, 0);
	}

	/**
	 * Returns the appropriate View that represents this field.
	 * <br>
	 * If not on a page,
	 * <ul>
	 * <li> if no media has been captured, or the capture UI has been specifically requested (e.g. when the 
	 * user wants to capture more media from the gallery), return the capture UI.</li>
	 * <li> else
	 * <ul>
	 * <li> if the field can only have one attachment and one has been made, return the single-item review UI. 
	 * Also return the single-item review UI if an item has been selected from the gallery. </li>
	 * <li> else (field max is larger than 1 and an item hasn't been selected), return the gallery UI.</li>
	 * </ul>
	 * </li>
	 * </ul>
	 * 
	 * TODO take "enabled" into account for non-page views(?)
	 */
	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		//Log.d(TAG, "getPlatformView()");
		
		if(onPage)
		{
			if(pageView == null)
				pageView = new OnPageView(collectorUI.getContext());
			// Enable/disable:
			pageView.setEnabled(enabled); // also sets up event listeners!
			return pageView;
		}
		
		//Log.d(TAG,"Current state: " + currentState.name());

		// Decide which UI must be shown and return the corresponding view:
		Context context = collectorUI.getContext();
		switch(getMode())
		{
			case CAPTURE:
			case CAPTURE_FROM_GALLERY:
				if(captureView == null)
					captureView = new CaptureView(context);
				else
					captureView.refresh();
				return captureView;
	
			case SINGLE_ITEM_REVIEW:
			case SINGLE_ITEM_REVIEW_FROM_GALLERY:
				if(reviewView == null)
					reviewView = new ReviewView(context);
				// Set file to review:
				reviewView.setReviewFile(getFileToReview());
				// Return view:
				return reviewView;
				
			case GALLERY:
			default:
				if(galleryView == null)
					galleryView = new GalleryView(context);
				else if(newRecord) // wipe the cache whenever newRecord is true
					galleryView.cache.clear();
				// Force the gallery to update its contents and its button:
				galleryView.refresh();
				// Return view:
				return galleryView;
		}
	}
	
	/**
	 * Custom rules for showing of back button
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowBack()
	 */
	@Override
	protected boolean isShowBack()
	{
		// Always hide the forward button when reviewing a single item: 
		if(isInSingleItemReviewMode())
			return false;
		// Always show back button if in capture UI and coming from gallery such that the user can always go back to the gallery (regardless of field history on the stack):
		if(getMode() == Mode.CAPTURE_FROM_GALLERY)
			return true;
		// Default behaviour:
		return super.isShowBack();
	}

	/**
	 * Custom rules for showing of cancel button
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowCancel()
	 */
	@Override
	protected boolean isShowCancel()
	{
		// Always hide the cancel button when reviewing a single item: 
		if(isInSingleItemReviewMode())
			return false;
		// Default behaviour:
		return super.isShowCancel();
	}

	/**
	 * Custom rules for showing of forward button
	 *
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.SelfLeavingFieldUI#isShowForward()
	 */
	@Override
	public boolean isShowForward()
	{
		// Always hide the forward button when reviewing a single item: 
		if(isInSingleItemReviewMode())
			return false;
		// Show forward when record is valid (i.e. at least 1 attachment when field is non-optional):
		return isValid(controller.getCurrentRecord());
	}
	
	/**
	 * Custom navigation behaviour
	 */
	@Override
	public boolean handleControlEvent(Control.Type controlType)
	{
		// When the users presses "back" in the capture UI and came from the gallery...
		if(controlType == Control.Type.Back && getMode() == Mode.CAPTURE_FROM_GALLERY)
		{	// Go back to gallery instead of previous field:
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			return true;
		}
		// Do not change control behaviour in any other case (other than removing the capture request)
		return false;
	}
	
	/**
	 * @return a new file to save capture result to
	 */
	protected File getNewCaptureFile()
	{
		this.captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider(), controller.getCurrentRecord()); // remember the file!
		return captureFile;
	}
	
	protected void handleCaptureError(Exception exception)
	{
		Log.e(TAG, "Media capture failed in field \"" + field.id + "\"", exception);
		attachMedia(null);
		captureFile = null;
		if(isValid(controller.getCurrentRecord()))
			controller.goForward(false);
		else
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_NO_STORAGE);
	}
	
	/**
	 * Handles a freshly captured media file (i.e. by attaching it to the current record) and takes the user to the next screen/field.
	 */
	public void handleCaptureSuccess()
	{
		// Register attachment:
		attachMedia(captureFile);
		
		// forget about the captureFile so it is not deleted upon cancel():
		captureFile = null; // !!!
		
		// Where to go next?:
		if(field.isShowReview() || !isValid(controller.getCurrentRecord()))
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE); // stay at field if review is enabled, or if the record is somehow not valid (e.g. due to non-existing mediaAttachement)
		else
			controller.goForward(true); // Continue...
	}
	
	/**
	 * When this field is cancelled, delete any half-captured media.
	 * It means the last or on-going capture has been implicitly discarded (e.g. when pressing 'back' during video recording).
	 * 
	 */
	@Override
	protected void cancel()
	{
		//Log.d(TAG, "cancel()");
		if(captureFile != null)
			controller.addLogLine("DELETE_TEMP_FILE", field.id, captureFile.getAbsolutePath());
		FileUtils.deleteQuietly(captureFile); // does nothing when null
		captureFile = null;
	}
	
	/**
	 * What to do when the capture button has been pressed.
	 */
	protected abstract void onCapture();
	
	/**
	 * Returns an appropriate {@link Item} to display the provided file in the gallery.
	 * 
	 * @param index - the index of the attachment (= position within the gallery)
	 * @param attachement - the attached media file
	 * @return an Item corresponding to the attachment
	 */
	protected abstract Item<?> getGalleryItem(int index, File attachement);

	/**
	 * Sets the background colour & padding on button {@link Item}s.
	 * 
	 * @param btn
	 * @return
	 */
	protected Item<?> applyButtonSpecs(Item<?> btn)
	{
		return btn
			.setBackgroundColor(fieldBackgroundColor)
			.setPaddingDip(AndroidControlsUI.CONTROL_PADDING_DIP);
	}
	
	/**
	 * Returns an Item representing the "capture" button.
	 * 
	 * @param context
	 * @return an Item representing "capture" button for the media.
	 */
	protected Item<?> getCaptureButton(Context context)
	{
		return applyButtonSpecs(generateCaptureButton(context));
	}
	
	/**
	 * Generate a media-specific "capture" button (e.g. microphone for audio and camera for photo).
	 * No need to set background colour and padding.
	 * 
	 * @param context
	 * @return
	 */
	protected abstract Item<?> generateCaptureButton(Context context);
	
	/**
	 * Return a "approve" button item to be used when reviewing a single media item. Will use XML-specified image if available,
	 * otherwise the default resource is used.
	 * 
	 * Can be overridden by the subclass if needed.
	 * 
	 * @param context
	 * @return the "discard" button item
	 */
	public Item<?> getApproveButton(Context context)
	{
		return applyButtonSpecs(collectorUI.getImageItemFromProjectFileOrResource(field.getApproveButtonImageRelativePath(), R.drawable.button_tick));
	}
	
	/**
	 * Return a "discard" button item to be used when reviewing a single media item. Will use XML-specified image if available,
	 * otherwise the default resource is used.
	 * 
	 * Can be overridden by the subclass if needed.
	 * 
	 * @param context
	 * @return the "discard" button item
	 */
	public Item<?> getDiscardButton(Context context)
	{
		return applyButtonSpecs(collectorUI.getImageItemFromProjectFileOrResource(field.getDiscardButtonImageRelativePath(), R.drawable.button_trash));
	}
	
	/**
	 * Creates the main content for the capture UI.
	 * Is only called once (upon CaptureView constructor).
	 * 
	 * @param context
	 * @return a {@code View} containing the content for capture mode.
	 */
	protected abstract View createCaptureContent(Context context);

	/**
	 * Creates the main content for the review UI.
	 * Is called again every time we go into single item review.
	 * 
	 * @param context
	 * @param mediaFile - the file to be reviewed.
	 * @return a {@code View} containing the content for review mode.
	 */
	protected abstract View getReviewContent(Context context, File mediaFile);
	
	/**
	 * LinearLayout that holds the capture UI.
	 * 
	 * @author benelliott, mstevens
	 */
	private class CaptureView extends LinearLayout implements OnClickListener
	{

		private final View contentView; // capture UI content (e.g. viewfinder)
		private final Runnable captureButtonAction; // capture button action
		
		private View captureButtonView; // capture button

		public CaptureView(Context context)
		{
			super(context);

			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			
			// set weight sum (capture UI content will expand to fill all 'spare' space:
			this.setWeightSum(1.0f);

			// add content:
			this.contentView = createCaptureContent(context);
			LinearLayout.LayoutParams contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.gravity = Gravity.CENTER_HORIZONTAL;
			contentParams.weight = 1.0f;
			addView(contentView, contentParams);

			// Add capture button:
			captureButtonAction = new Runnable()
			{
				@Override
				public void run()
				{
					// Log interaction:
					controller.addLogLine("CAPTURE_MEDIA", field.id);
					
					// force gallery to check for new files next time it is entered (do this here as gallery may be entered in onCapture()):
					mediaItemsChanged = true;
					// execute subclass's capture behaviour and make note of whether or not to unblock UI afterwards
					onCapture();
					// refresh capture button on press (e.g. might need to change from "record" to "stop recording")
					refreshCaptureButton();
				}
			};
			
			// Init button:
			refreshCaptureButton();
		}
		
		public void refresh()
		{
			contentView.invalidate(); // force update of the content view
			refreshCaptureButton();
		}
		
		/**
		 * (Re)create the capture button
		 */
		protected void refreshCaptureButton()
		{
			this.removeView(captureButtonView);
			captureButtonView = getCaptureButton(getContext()).getView(getContext());
			captureButtonView.setOnClickListener(this);
			this.addView(captureButtonView, buttonParams);
		}

		@Override
		public void onClick(View v)
		{
			if(v == captureButtonView) // check just in case
				collectorUI.clickView(v, captureButtonAction, unblockUIAfterCaptureClick);
		}
		
	}
	
	/**
	 * LinearLayout that holds the single-item review UI.
	 * 
	 * @author benelliott, mstevens
	 */
	private class ReviewView extends LinearLayout implements OnItemClickListener
	{

		private final LinearLayout.LayoutParams contentParams;
		private final Runnable approveAction;
		private final Runnable discardAction;

		private File toReview;
		
		private ReviewView(Context context)
		{
			super(context);
			
			// Layout:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);
			
			// Content layoutparams:
			contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.weight = 1.0f;
			contentParams.gravity = Gravity.CENTER;
		
			// Button container:
			ItemPickerView reviewButtons = new ItemPickerView(context, true);
			reviewButtons.setNumColumns(2);
			reviewButtons.setBackgroundColor(Color.BLACK);
			reviewButtons.setHorizontalSpacing(collectorUI.getSpacingPx());
			reviewButtons.setItemDimensionsPx(LayoutParams.MATCH_PARENT, collectorUI.getControlHeightPx());
			reviewButtons.setOnItemClickListener(this);
			addView(reviewButtons, buttonParams);
			
			// Approve button:
			approveAction = makeButtonAction(false);
			reviewButtons.getAdapter().addItem(getApproveButton(context));
			
			// Discard button:
			discardAction = makeButtonAction(true);
			reviewButtons.getAdapter().addItem(getDiscardButton(context));
		}
		
		public void setReviewFile(File toReview)
		{
			if(this.toReview != null)
				removeViewAt(0);
			addView(getReviewContent(getContext(), toReview), 0, contentParams);
			this.toReview = toReview;
		}
		
		@Override
		public void onItemClick(final AdapterView<?> parent, final View view, final int position, final long id)
		{
			collectorUI.clickView(view, position == 0 ? approveAction : discardAction);
		}
		
		private Runnable makeButtonAction(final boolean discarding)
		{
			return new Runnable()
			{
				@Override
				public void run()
				{
					controller.addLogLine("REVIEW_MEDIA", field.id, discarding ? "DISCARD" : "APPROVE", toReview != null ? toReview.getName() : "?");
					
					if(discarding)
					{
						// Delete attachment:
						removeMedia(ReviewView.this.toReview);
						
						// If there is a gallery cache (meaning we have opened the gallery before) then delete the corresponding cached item:
						if(galleryView != null)
							galleryView.cache.remove(ReviewView.this.toReview);
						
						// An item has been deleted, so we want the gallery to be refreshed:
						mediaItemsChanged = true;
					}
					
					// Determine what to do next...
					if(discarding || getMode() == Mode.SINGLE_ITEM_REVIEW_FROM_GALLERY)
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE); // will go back to capture (when the 1 and only item was deleted), or back to gallery
					else // single item approval:
						controller.goForward(true);
				}
			};
		}

	}
	
	/**
	 * LinearLayout that holds the gallery UI (only used when there are multiple media attachments to show).
	 * 
	 * @author benelliott, mstevens
	 */
	private class GalleryView extends LinearLayout implements OnItemClickListener, OnClickListener
	{

		private final ItemPickerView pickerView;
		
		/**
		 * Map used to cache gallery items so that every attachment doesn't have to be reloaded when a change is made
		 */
		private final Map<File, Item<?>> cache;
		
		private View addButtonView;
		private final Runnable addButtonAction;

		private static final int NUM_COLUMNS = 3; // TODO make configurable?
		private static final int NUM_ROWS = 3;
		
		private GalleryView(Context context)
		{
			super(context);
			
			// Init cache:
			cache = new HashMap<File, Item<?>>();
			
			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);

			// Add gallery picker:
			pickerView = new ItemPickerView(context);
			LinearLayout.LayoutParams contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.weight = 1.0f;
			this.addView(pickerView, contentParams);
			//	Number of columns:
			pickerView.setNumColumns(NUM_COLUMNS);
			//	Item size & padding:
			pickerView.setItemDimensionsPx(collectorUI.getFieldUIPartWidthPx(NUM_COLUMNS), collectorUI.getFieldUIPartHeightPx(NUM_ROWS));
			//	Item spacing:
			int spacingPx = collectorUI.getSpacingPx();
			pickerView.setHorizontalSpacing(spacingPx);
			pickerView.setVerticalSpacing(spacingPx);
			// When a gallery item is clicked, present it in full screen for review/deletion:
			pickerView.setOnItemClickListener(this);
			
			// Add button:
			addButtonAction = new Runnable()
			{
				@Override
				public void run()
				{
					// "capture more" button clicked, return to capture interface...
					if(!field.isMaxAttachmentsReached(controller.getCurrentRecord()))
					{
						// add the "go to capture" argument to the field:
						controller.getCurrentFieldArguments().put(GO_TO_CAPTURE_KEY, "true");
						// re-enter current field:
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
					}
					// allow clicks now we have finished
					controller.unblockUI();
				}
			};
		}
		
		/**
		 * Refreshes the contents of the gallery and the "capture" button.
		 */
		public void refresh()
		{
			// Update gallery contents & capture button if necessary...
			PickerAdapter adapter = pickerView.getAdapter();
			if(mediaItemsChanged)
			{	// An item has been added or deleted so reload gallery items:
				//Log.d(TAG, "Media items changed, updating gallery...");
				
				adapter.clear(); // reset adapter
				int f = 0;
				for(File file : field.getAttachments(controller.getFileStorageProvider(), controller.getCurrentRecord()))
				{
					// See if it can be found in the gallery cache:
					Item<?> toAdd = cache.get(file);
					if(toAdd == null)
					{	// If not, create a new item and put it in the cache:
						toAdd = getGalleryItem(f++, file);
						toAdd.setBackgroundColor(fieldBackgroundColor);
						cache.put(file, toAdd);
					}
					// Set padding and add item to adapter:
					toAdd.setPaddingDip(CollectorView.PADDING_DIP);
					adapter.addItem(toAdd);
				}
				
				// Refresh capture button in case it is now (un-)greyed out:
				this.removeView(addButtonView);
				addButtonView = createCaptureMoreButton().getView(getContext());
				addButtonView.setOnClickListener(this);
				addButtonView.setEnabled(!field.isMaxAttachmentsReached(controller.getCurrentRecord()));
				this.addView(addButtonView, buttonParams);
			}
			// Force the picker to update its views (do this regardless of mediaItemsChanged else there will be UI glitches)
			pickerView.setAdapter(adapter);
			mediaItemsChanged = false; // !!!
		}
		
		/**
		 * Creates a "capture more" button, but makes it appear grayed out if the field's maximum number of attachments has been reached.
		 * 
		 * @return the "capture more" button to be added to the UI.
		 */
		private Item<?> createCaptureMoreButton()
		{
			Item<?> captureButton = getCaptureButton(getContext());
			return !field.isMaxAttachmentsReached(controller.getCurrentRecord()) ?
				captureButton :						// return capture button as is
				ItemHelpers.GrayOut(captureButton); // make button look grayed-out
		}
		
		@Override
		public void onItemClick(AdapterView<?> parent, View view, final int position, long id)
		{
			collectorUI.clickView(view, new Runnable()
			{
				@Override
				public void run()
				{
					controller.getCurrentFieldArguments().put(	REVIEW_FILE_PATH_KEY,
																field.getAttachment(controller.getFileStorageProvider(), controller.getCurrentRecord(), position).getAbsolutePath());
					controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
				}
			});
		}

		@Override
		public void onClick(View v)
		{
			if(v == addButtonView) // check just in case
				collectorUI.clickView(v, addButtonAction);
		}
		
	}
	
	/**
	 * View for displaying a MediaField as part of a Page
	 * 
	 * @author mstevens, benelliott
	 */
	public class OnPageView extends LinearLayout implements OnClickListener, OnFocusChangeListener
	{
		
		static public final float PAGE_ITEM_SIZE_DIP = 60.0f; // width = height
		static public final float PAGE_ITEM_MARGIN_DIP = 1.0f; // same margin all round

		private TextView label;
		private View captureView;
		private int captureSizePx;
		private int captureMarginPx;

		public OnPageView(Context context)
		{
			super(context);
			this.setOrientation(LinearLayout.VERTICAL);

			// Add label:
			label = new TextView(getContext());
			label.setText(field.getCaption());
			// ensure that the label text is not truncated, by setting width to WRAP_CONTENT:
			label.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
			this.addView(label);

			captureSizePx = ScreenMetrics.ConvertDipToPx(context, PAGE_ITEM_SIZE_DIP);
			captureMarginPx = ScreenMetrics.ConvertDipToPx(context, PAGE_ITEM_MARGIN_DIP);

			// just use the capture button image to represent the media field:
			captureView = getCaptureButton(context).getView(context);

			// Set margins on layoutparams:
			LayoutParams captureLP = new LinearLayout.LayoutParams(captureSizePx, captureSizePx);
			captureLP.setMargins(captureMarginPx, captureMarginPx, captureMarginPx, captureMarginPx);

			// Add the view:
			this.addView(captureView, captureLP);
		}

		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			if(captureView != null)
			{
				captureView.setEnabled(enabled);
				captureView.setOnClickListener(enabled ? this : null);
				// Make other fields lose focus and simulate clicking with onFocusChange:
				captureView.setFocusable(enabled);
				captureView.setFocusableInTouchMode(enabled);
				captureView.setOnFocusChangeListener(enabled ? this : null);
			}
		}

		/**
		 * TODO is this really necessary? 
		 * 
		 * @see android.view.View.OnFocusChangeListener#onFocusChange(android.view.View, boolean)
		 */
		@Override
		public void onFocusChange(View v, boolean hasFocus)
		{
			if(hasFocus && isFieldShown() && isEnabled() && v.isEnabled())
			{
				// Lose focus again:
				v.clearFocus();
				
				// Simulate click:
				onClick(v);
			}
		}

		@Override
		public void onClick(View v)
		{
			// Do nothing if not shown or enabled:
			if(!isFieldShown() || !isEnabled() || !v.isEnabled())
				return;
			
			clearPageInvalidMark();
			
			// Task to perform after animation has finished:
			Runnable action = new Runnable()
			{
				public void run()
				{
					controller.goTo(new FieldWithArguments(field), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the field itself
				}
			};

			// Perform the click
			collectorUI.clickView(v, action);
		}
		
	}
	
}
