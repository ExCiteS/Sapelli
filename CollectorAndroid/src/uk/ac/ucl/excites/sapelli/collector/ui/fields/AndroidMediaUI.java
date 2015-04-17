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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.LinearLayout;
import android.widget.TextView;

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
	protected static enum DisplayState
	{
		CAPTURE,
		CAPTURE_FROM_GALLERY,
		GALLERY,
		SINGLE_ITEM_REVIEW, // alternative to gallery mode when field.max = 1
		SINGLE_REVIEW_FROM_GALLERY // showing a single item from gallery
	};

	protected static final String TAG = "AndroidMediaUI";
	
	// Keys to use when obtaining values from field arguments:
	private static final String REVIEW_FILE_PATH_KEY = "REVIEW_FILE_PATH";
	private static final String GO_TO_CAPTURE_KEY = "GO_TO_CAPTURE";

	// DYNAMIC ------------------------------------------------------
	private boolean mediaItemsChanged = false; 	// whether or not the media items have been changed (whether the gallery needs to be redrawn)
	protected File captureFile; // file used to store media while it is being captured

	protected final boolean hasPreCaptureView;
	protected final boolean unblockUIAfterCaptureClick;
	
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
	 * @param hasPreCaptureView whether or not the capture button should be maximised when the capture UI is first entered
	 * @param unblockUIAfterCaptureClick indicates whether or not to immediately allow new user interactions (i.e. clicks) after the capture button is clicked
	 * after this method has returned.
	 */
	public AndroidMediaUI(MF field, CollectorController controller, CollectorView collectorUI, boolean hasPreCaptureView, boolean unblockUIAfterCaptureClick)
	{
		super(field, controller, collectorUI);
		
		this.hasPreCaptureView = hasPreCaptureView;
		this.unblockUIAfterCaptureClick = unblockUIAfterCaptureClick;
		
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
	 */
	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		// TODO take "enabled" into account
		if(onPage)
		{
			if(pageView == null)
				pageView = new OnPageView(collectorUI.getContext());
			
			// Enable/disable:
			pageView.setEnabled(enabled); // also sets up event listeners!
			return pageView;
		}

		// Get the current display state:
		DisplayState currentState = getCurrentDisplayState();
		
		//Log.d(TAG,"Current state: " + currentState.name());

		// Decide which UI to return and return it:
		switch(currentState)
		{
			case CAPTURE:
			case CAPTURE_FROM_GALLERY:
				if(captureView == null)
					captureView = new CaptureView(collectorUI.getContext());
				captureView.refreshCaptureButton();
				// keep a reference to the capture UI so the capture button can later be maximised/minimised
				return captureView;
	
			case SINGLE_ITEM_REVIEW:
			case SINGLE_REVIEW_FROM_GALLERY:
				if(reviewView == null)
					reviewView = new ReviewView(collectorUI.getContext());				
				// Set file to review:
				reviewView.setReviewFile(currentState == DisplayState.SINGLE_ITEM_REVIEW ?
					field.getLastAttachment(controller.getFileStorageProvider(), record) /*post-capture*/ :
					new File(controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY)) /*from-gallery*/);
				// Return view:
				return reviewView;
				
			case GALLERY:
			default:
				if(galleryView == null)
					galleryView = new GalleryView(collectorUI.getContext());
				else if(newRecord) // wipe the cache whenever newRecord is true
					galleryView.cache.clear();
				// Force the gallery to update its contents and its button:
				galleryView.refresh();
				// Return view:
				return galleryView;
		}
	}

	/**
	 * @return the current display state based on the field's arguments and properties.
	 */
	private DisplayState getCurrentDisplayState()
	{
		if(controller.getCurrentFieldArguments().getBoolean(GO_TO_CAPTURE_KEY, false))
			// we have been explicitly told to go to capture state (by means of the "add more" button in the gallery):
			return DisplayState.CAPTURE_FROM_GALLERY;
		
		if(field.getAttachmentCount(controller.getCurrentRecord()) == 0)
			// we have no attachments yet:
			return DisplayState.CAPTURE;

		if(field.getMax() == 1)
			// not in capture and can have max. 1 attachment, so go to single item review
			return DisplayState.SINGLE_ITEM_REVIEW;

		if(controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY) != null)
			// not in capture, max > 1, and there was a filepath in the arguments so show that file for review
			return DisplayState.SINGLE_REVIEW_FROM_GALLERY;

		// have multiple attachments, not going to capture, and no filepath for review so go to gallery
		return DisplayState.GALLERY;
	}

	/**
	 * Ensures that the media file reference is nullified after a capture is made so that it is not deleted
	 * from the {@link AndroidMediaUI#cancel()} method.
	 */
	@Override
	public void attachMedia(File mediaAttachment)
	{
		super.attachMedia(mediaAttachment);
		captureFile = null;
	}

	/**
	 * When this field is cancelled, delete any half-captured media.
	 * It means the last or on-going capture has been implicitly discarded (e.g. when pressing 'back' during video recording).
	 * 
	 */
	@Override
	protected void cancel()
	{
		//Log.d(TAG, "Deleting discarded file...");
		FileUtils.deleteQuietly(captureFile);
		captureFile = null;
	}

	/**
	 * Force the back button to be shown if in single item review (from gallery) so that the user can always go back
	 * to the gallery (regardless of field history on the stack).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowBack()
	 */
	@Override
	protected boolean isShowBack()
	{
		DisplayState state =  getCurrentDisplayState();
		if(	state == DisplayState.SINGLE_REVIEW_FROM_GALLERY ||
			state == DisplayState.CAPTURE_FROM_GALLERY)
			return true;
		return super.isShowBack();
	}

	/**
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowCancel()
	 */
	@Override
	protected boolean isShowCancel()
	{
		if(getCurrentDisplayState() == DisplayState.SINGLE_ITEM_REVIEW)
			return false;
		return super.isShowCancel();
	}

	/**
	 *
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.SelfLeavingFieldUI#isShowForward()
	 */
	@Override
	public boolean isShowForward()
	{
		// Hide the forward button when reviewing a single item from the gallery: 
		if(getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
			return false;
		return super.isShowForward();
	}
	
	/**
	 * Override the normal "back" behaviour so that the user is returned to the gallery when "back" is pressed
	 * from the single-item review UI.
	 */
	@Override
	public boolean handleControlEvent(Control.Type controlType)
	{
		DisplayState state =  getCurrentDisplayState();

		// Handle back press in certain cases ...
		if(	controlType.equals(Control.Type.Back) &&
			(state == DisplayState.SINGLE_REVIEW_FROM_GALLERY || state == DisplayState.CAPTURE_FROM_GALLERY))
		{
			if(state == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
				// Remove the filepath from the field's arguments so we do not re-enter single-item review unintentionally
				controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY);
			else
				// To avoid we go back to capture:
				controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY);
			
			// Go back to gallery instead of previous field:
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			
			return true;
		}
		
		// Do not change the button behaviour in any other case (other than removing the capture request)
		return false;
	}

	protected void minimiseCaptureButton()
	{
		if(captureView != null)
			captureView.refreshCaptureButton(false);
	}

	protected void maximiseCaptureButton()
	{
		if(captureView != null)
			captureView.refreshCaptureButton(true);
	}
	
	/**
	 * What to do when the capture button has been pressed.
	 */
	protected abstract void onCapture();

	/**
	 * What to do when a piece of media is discarded after review (e.g. release media player resources).
	 */
	protected abstract void onLeaveReview();
	
	/**
	 * Returns an appropriate {@link Item} for the provided file.
	 * 
	 * @param index - the index of the attachment (= position within the gallery)
	 * @param attachement - the attached media file
	 * @return an Item corresponding to the attachment
	 */
	protected abstract Item<?> getItemForAttachment(int index, File attachement);

	/**
	 * Generate a "capture" button (may vary by media, e.g. microphone
	 * for audio and camera for photo).
	 * @param context
	 * @return an ImageItem housing an appropriate "capture" button for the media.
	 */
	protected abstract Item<?> generateCaptureButton(Context context);
	
	/**
	 * Return a "approve" button item to be used when reviewing a single media item. Will use XML-specified image if available,
	 * otherwise the default resource is used.
	 * Can be overridden by the subclass if needed.
	 * 
	 * @param context
	 * @return the "discard" button item
	 */
	public Item<?> generateApproveButton(Context context)
	{
		Item<?> approveButton = null;
		File imgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getApproveButtonImageRelativePath());
		if(FileHelpers.isReadableFile(imgFile))
			approveButton = new FileImageItem(imgFile);
		else
			approveButton = new ResourceImageItem(context.getResources(), R.drawable.button_tick_svg);
		approveButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return approveButton;
	}
	
	/**
	 * Return a "discard" button item to be used when reviewing a single media item. Will use XML-specified image if available,
	 * otherwise the default resource is used.
	 * Can be overridden by the subclass if needed.
	 * 
	 * @param context
	 * @return the "discard" button item
	 */
	public Item<?> generateDiscardButton(Context context)
	{
		Item<?> discardButton = null;
		File discardImgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getDiscardButtonImageRelativePath());
		if(FileHelpers.isReadableFile(discardImgFile))
			discardButton = new FileImageItem(discardImgFile);
		else
			discardButton = new ResourceImageItem(context.getResources(), R.drawable.button_trash_svg);
		discardButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return discardButton;
	}
	
	/**
	 * Creates the main content for the capture UI.
	 * @param context
	 * @return a {@code View} containing the content for capture mode.
	 */
	protected abstract View getCaptureContent(Context context);

	/**
	 * Creates the main content for the review UI.
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
		private boolean captureButtonMaximised = false;

		private CaptureView(Context context)
		{
			super(context);

			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			
			// set weight sum (capture UI content will expand to fill all 'spare' space:
			this.setWeightSum(1.0f);

			// add content:
			this.contentView = getCaptureContent(context);
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
					// just made a capture, so go to gallery/review when the field is re-entered:
					controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY);
					// force gallery to check for new files next time it is entered (do this here as gallery may be entered in onCapture()):
					mediaItemsChanged = true;
					// execute subclass's capture behaviour and make note of whether or not to unblock UI afterwards
					onCapture();
					// refresh capture button on press (e.g. might need to change from "record" to "stop recording")
					refreshCaptureButton();
				}
			};
		}
		
		/**
		 * Recreate the capture button, without changing maximisation state
		 */
		protected void refreshCaptureButton()
		{
			refreshCaptureButton(captureButtonMaximised);
		}
		
		/**
		 * Recreate and maximise the capture button
		 */
		protected void refreshCaptureButton(boolean maximise)
		{
			// Hide captureView when maximised:
			contentView.setVisibility(maximise ? View.GONE : View.VISIBLE);
			
			this.removeView(captureButtonView);
			captureButtonView = generateCaptureButton(getContext()).getView(getContext());
			captureButtonView.setOnClickListener(this);
			this.addView(captureButtonView, maximise ? new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT) : buttonParams);
			
			// Remember maximisation:
			captureButtonMaximised = maximise;
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
			reviewButtons.getAdapter().addItem(generateApproveButton(context));
			
			// Discard button:
			discardAction = makeButtonAction(true);
			reviewButtons.getAdapter().addItem(generateDiscardButton(context));
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
					onLeaveReview();
					
					controller.addLogLine("CLICK_MEDIA_" + (discarding ? "DISCARD" : "APPROVE"));
					
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
					
					// Remove the filepath from the field's arguments to avoid that we re-enter single-item review unintentionally:
					controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY);
					
					if(discarding || getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
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
					if(!field.isMaxReached(controller.getCurrentRecord()))
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
			if (mediaItemsChanged)
			{
				// An item has been added or deleted so reload gallery items					
				//Log.d(TAG, "Media items changed, updating gallery...");
				
				adapter.clear(); // reset adapter
				int f = 0;
				for(File file : field.getAttachments(controller.getFileStorageProvider(), controller.getCurrentRecord()))
				{
					// See if it can be found in the gallery cache:
					Item<?> toAdd = cache.get(file);
					if(toAdd == null)
					{	// If not, create a new item and put it in the cache:
						toAdd = getItemForAttachment(f++, file);
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
				this.addView(addButtonView, buttonParams);
			}
			// Force the picker to update its views (do this regardless of mediaItemsChanged else there will be UI glitches)
			pickerView.setAdapter(adapter);
			mediaItemsChanged = false; // !!!
		}
		
		/**
		 * Creates a "capture more" button to be used in the picker, but makes it appear grayed out
		 * if the field's maximum number of attachments has been reached.
		 * 
		 * @return the "capture more" button to be added to the UI.
		 */
		private Item<?> createCaptureMoreButton()
		{
			// creates a "normal" capture button and then disables it if max reached.
			Item<?> captureButton = generateCaptureButton(getContext());

			if(!field.isMaxReached(controller.getCurrentRecord()))
				return captureButton;

			// else make button look unclickable
			LayeredItem layeredItem = new LayeredItem();
			layeredItem.addLayer(captureButton);

			// Make background of layered stack gray:
			layeredItem.setBackgroundColor(CollectorView.COLOR_GRAY);
			// Add grayed-out layer:
			Item<?> grayOutOverlay = new EmptyItem();
			grayOutOverlay.setBackgroundColor(CollectorView.COLOR_MOSTLY_OPAQUE_GRAY);
			layeredItem.addLayer(grayOutOverlay);

			return layeredItem;
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
			captureView = generateCaptureButton(context).getView(context);

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
