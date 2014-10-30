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
import java.util.List;
import java.util.concurrent.Semaphore;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;

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
	
	protected static enum DisplayState {
		CAPTURE,
		GALLERY,
		SINGLE_ITEM_REVIEW, // alternative to gallery mode when field.max = 1
		SINGLE_REVIEW_FROM_GALLERY // showing a single item from gallery
	};
	
	private static final String TAG = "AndroidMediaUI";
	private static final String REVIEW_FILE_PATH_KEY = "REVIEW_FILE_PATH"; // key for obtaining review filepath from field arguments
	private static final String GO_TO_CAPTURE_KEY = "GO_TO_CAPTURE";

	protected Semaphore handlingClick; // semaphore used to prevent race conditions on click events
	private boolean maxReached; // whether or not the maximum number of pieces of media have been captured for this field
	private boolean mediaItemsChanged = false; 	// whether or not the media items have been changed (whether the gallery needs to be redrawn)
	protected File captureFile; // file used to store media while it is being captured

	// Three possible UI screens:
	private CaptureView captureView;
	private ReviewView reviewView;
	private GalleryView galleryView;


	public AndroidMediaUI(MF field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		maxReached = (field.getCount(controller.getCurrentRecord()) >= field.getMax());
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
		//TODO take "enabled" into account
		if(onPage)
		{
			//TODO
			return null;
		}

		// initialise click semaphore:
		if (handlingClick == null)
			handlingClick = new Semaphore(1);

		DisplayState currentState = getCurrentDisplayState();
		File toReview;

		switch (currentState) {
		case CAPTURE:
			
			if (captureView == null)
				captureView = new CaptureView(collectorUI.getContext());

			// populate capture layout (e.g. set up photo viewfinder)
			populateCaptureLayout(captureView.contentView);
			
			return captureView;

		case SINGLE_ITEM_REVIEW:

			if (reviewView == null)
				reviewView = new ReviewView(collectorUI.getContext());
			// populate review layout with most recent capture:
			toReview = field.getLastAttachment(controller.getFileStorageProvider(), record);
			reviewView.setReviewFile(toReview);
			populateReviewLayout(reviewView.contentView, toReview);
			
			return reviewView;

		case SINGLE_REVIEW_FROM_GALLERY:

			if (reviewView == null)
				reviewView = new ReviewView(collectorUI.getContext());
			// populate review layout with provided filepath:
			toReview = new File(controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY));
			reviewView.setReviewFile(toReview);
			populateReviewLayout(reviewView.contentView, toReview);

			// remove the filepath from the field's arguments so we do not re-enter single-item review unintentionally
			controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY);
			
			return reviewView;

		default: // Return gallery
			
			if (galleryView == null)
				galleryView = new GalleryView(collectorUI.getContext());
			// force the gallery to update its contents and its button:
			galleryView.refresh();
			
			return galleryView;
		}
	}

	/**
	 * Return the current display state based on the field's arguments and properties.
	 * @return
	 */
	private DisplayState getCurrentDisplayState() {
		if (field.getCount(controller.getCurrentRecord()) == 0 || controller.getCurrentFieldArguments().getBoolean(GO_TO_CAPTURE_KEY, false)) {
			// either have no files to review or have been explicitly told to go to capture state
			return DisplayState.CAPTURE;
		}
		
		if (field.getMax() == 1)
			// not in capture and can have max. 1 attachment, so go to single item review
			return DisplayState.SINGLE_ITEM_REVIEW;
		
		if (controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY) != null)
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
    public void attachMedia(File mediaAttachment) {
		super.attachMedia(mediaAttachment);
		captureFile = null; 
	}

	/**
	 * When this field is cancelled, delete any half-captured media (e.g. pressing 'back' 
	 * during video recording).
	 */
	@Override
	protected void cancel()
	{
		if (captureFile != null) {
			// last capture has been implicitly discarded.
			Log.d(TAG, "Deleting discarded file...");
			captureFile.delete();
		}
		captureFile = null;

	}


	/**
	 * Override the normal "back" behaviour so that the user is returned to the gallery when "back" is pressed
	 * from the single-item review UI.
	 */
	@Override
	public boolean handleControlEvent(Control control) {
		if (control.equals(Control.BACK) && getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY) {
			// we are currently in single-item review from gallery, so back button must just return us to gallery
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			return true;
		}
		// do not change the button behaviour in any other case
		return false;
	}
	
	/**
	 * Force the back button to be shown if in single item review (from gallery) so that the user can always go back
	 * to the gallery (regardless of field history on the stack).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowBack()
	 */
	@Override
    protected boolean isShowBack() {
		if (getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
			return true;
		return super.isShowBack();
    }

	/**
	 * Hide the forward button when reviewing a single item from the gallery.
	 *
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.SelfLeavingFieldUI#isShowForward()
	 */
	@Override
	public boolean isShowForward() {
		if (getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
			return false;
		return super.isShowForward();
	}

	protected Context getContext() {
		return collectorUI.getContext();
	}
	
	protected void minimiseCaptureButton() {
		if (captureView != null)
			captureView.minimiseCaptureButton();
	}
	
	protected void maximiseCaptureButton() {
		if (captureView != null)
			captureView.maximiseCaptureButton();
	}
	
	// -------- ABSTRACT METHODS:

	/**
	 * What to do when the capture button has been pressed.
	 */
	protected abstract void onCapture();

	/**
	 * What to do when a piece of media is discarded after review (e.g. release media player resources).
	 */
	protected abstract void onDiscard();

	/**
	 * @return a list of the media items captured in this field. Left to the subclass to implement because
	 * different media types should be used to instantiate different media items (e.g. if video, create a list
	 * of VideoItems).
	 */
	protected abstract List<Item> getMediaItems(FileStorageProvider fileStorageProvider, Record record);

	/**
	 * Generate a "capture" button (may vary by media, e.g. microphone
	 * for audio and camera for photo).
	 * @param context
	 * @return an ImageItem housing an appropriate "capture" button for the media.
	 */
	protected abstract ImageItem generateCaptureButton(Context context);

	/**
	 * Populate a container with views as appropriate to create an interface
	 * for media capture (e.g. viewfinder for camera).
	 * @param captureLayout - the container to populate.
	 */
	protected abstract void populateCaptureLayout(ViewGroup captureLayout);

	/**
	 * Populate a container with views as appropriate to create an interface for 
	 * the review and deletion of the media file provided (e.g. full-page photo).
	 * @param deleteLayout - the container to populate.
	 * @param mediaFile - the media file being reviewed.
	 */
	protected abstract void populateReviewLayout(ViewGroup reviewLayout, File mediaFile);
	
	// -------- PRIVATE CLASSES:

	/**
	 * Abstract class that represents the bottom row of control buttons that is
	 * present on any media UI page.
	 * 
	 * @author mstevens, benelliott
	 */
	private abstract class MediaButtonView extends PickerView
	{
		private int buttonPadding;
		private int buttonBackColor;
		Runnable buttonAction;

		public MediaButtonView(Context context)
		{
			// ensure views are not recycled or some changes to the buttons may not propagate correctly:
			super(context, false);

			this.setOnItemClickListener(new OnItemClickListener() {
				@Override
				public void onItemClick(AdapterView<?> parent, View view,
						int position, long id) {
					if (handlingClick.tryAcquire()) {
						/* NOTE: try to acquire semaphore here rather than in Runnable,
							 because Runnable may be delayed by animation (e.g. press "capture",
							 then move to review, but "capture" Runnable checks semaphore after
							 View has changed. Should still release it in the Runnable for a
							 similar reason. */
						// Execute the "press" animation if allowed, then perform the action: 
						if(controller.getCurrentForm().isClickAnimation())
							(new ClickAnimator(buttonAction, view, collectorUI)).execute(); //execute animation and the action afterwards
						else
							buttonAction.run(); //perform task now (animation is disabled)
					}
				}	
			});

			// Layout:
			setBackgroundColor(Color.TRANSPARENT);
			setGravity(Gravity.CENTER);
			setPadding(0, collectorUI.getSpacingPx(), 0, 0);
			this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));

			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP * 3);
			this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);
			// Columns:
			setNumColumns(getNumberOfColumns());
			
			// The addButtons() should be called after the button parameters (size, padding etc.) have been setup
			addButtons();

			// And finally:
			setAdapter(getAdapter()); // this is supposedly needed on Android v2.3.x (TODO test it)
		}

		/**
		 * Add a single item to the adapter.
		 * 
		 * @param button - the item to add.
		 */
		protected void addButton(Item button)
		{
			Log.d(TAG, "Adding button, height: "+this.itemLayoutParams.height);
			button.setPaddingPx(buttonPadding);
			button.setBackgroundColor(buttonBackColor);
			getAdapter().addItem(button);
		}

		protected abstract int getNumberOfColumns();

		/**
		 * Add all necessary buttons to the picker.
		 */
		protected abstract void addButtons();
		
		protected void refreshButtons() {
			getAdapter().clear();
			addButtons();
		}

	}
	
	// -------------- CAPTURE UI:

	/**
	 * LinearLayout that holds the capture UI.
	 * @author benelliott
	 *
	 */
	private class CaptureView extends LinearLayout {
				
		private CaptureContentView contentView; // capture UI content
		private CaptureButtonView buttonView; // capture button
		private boolean captureButtonMaximised = false;
		private Context context;
		
		private CaptureView(Context context) {
			super(context);
			this.context = context;
			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			// set weight sum (capture UI content will expand to fill all 'spare' space:
			this.setWeightSum(1.0f);
			// add content:
			contentView = new CaptureContentView(context);
			this.addView(contentView);
			// add button:
			buttonView = new CaptureButtonView(context);
			this.addView(buttonView);
		}
		
		/**
		 * Restores a maximised capture button to its original position at the bottom of the screen.
		 */
		protected void minimiseCaptureButton() {
			if (captureButtonMaximised) {
				captureButtonMaximised = false;
				// show all other content from capture UI:
				contentView.setVisibility(View.VISIBLE);
				// restore capture button's original (minimised) layout parameters:
				buttonView.setPadding(0, collectorUI.getSpacingPx(), 0, 0);
				buttonView.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
				// redraw the capture button:
				buttonView.refreshButtons();
			}
		}

		/**
		 * Expands a minimised capture button to take up the entire capture UI (e.g. for audio recording).
		 */
		protected void maximiseCaptureButton() {
			if (!captureButtonMaximised) {
				captureButtonMaximised = true;
				// hide all other content from capture UI:
				contentView.setVisibility(View.GONE);
				// fill the capture UI with the capture button:
				buttonView.setPadding(0, 0, 0, 0);
				buttonView.setItemDimensionsPx(LayoutParams.MATCH_PARENT, collectorUI.getFieldUIHeightPx());
				// redraw the capture button:
				buttonView.refreshButtons();
			}
		}
		
		/**
		 * LinearLayout that holds the capture UI content (to be populated by AndroidMediaUI subclass).
		 * @author benelliott
		 *
		 */
		private class CaptureContentView extends LinearLayout {
			
			private CaptureContentView(Context context) {
				super(context);
				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
				// expand to fill all available space:
				params.weight = 1.0f;
				this.setLayoutParams(params);
				this.setOrientation(LinearLayout.VERTICAL);
			}
		}
		
		/**
		 * The View that holds the capture button, at the bottom of the
		 * Capture UI.
		 * 
		 * @author mstevens, benelliott
		 *
		 */
		private class CaptureButtonView extends MediaButtonView
		{

			public CaptureButtonView(Context context)
			{
				super(context);
				
				buttonAction = new Runnable() {
					@Override
					public void run() {
						controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY); //just made a capture, so go to gallery instead (unless multiple disabled)
						onCapture();
						CaptureButtonView.this.refreshButtons();
						mediaItemsChanged = true; 
					}
				};
				
			}

			@Override
			protected int getNumberOfColumns()
			{
				return 1;
			}

			/**
			 * Note: for now there is only the capture button, we might add other features (flash, zooming, etc.) later
			 * 
			 * @see uk.ac.ucl.excites.sapelli.collector.ui.MediaButtonView.CameraView.CameraButtonView#addButtons()
			 */
			@Override
			protected void addButtons()
			{
				// Capture button:
				addButton(generateCaptureButton(getContext()));
			}
			
		}
		
	}
	
	/**
	 * LinearLayout that holds the single-item review UI.
	 * @author benelliott
	 *
	 */
	private class ReviewView extends LinearLayout {
		
		private ReviewContentView contentView; // review UI content
		private ReviewButtonView buttonView; // "delete item" button
		private File toReview; // file used to populate review UI
		
		private ReviewView(Context context) {
			super(context);
			// all XML stuff
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);
			
			// all init stuff
			contentView = new ReviewContentView(context);
			this.addView(contentView);
			
			buttonView = new ReviewButtonView(context);
			this.addView(buttonView);
		}
		
		/**
		 * Give this object a handle to the file that is being reviewed so that it can delete it 
		 * when the delete button is pressed.
		 * 
		 * @param toReview
		 */
		private void setReviewFile(File toReview) {
			this.toReview = toReview;
		}
		
		/**
		 * LinearLayout that holds the review UI content (to be populated by AndroidMediaUI subclass).
		 * 
		 * @author benelliott
		 *
		 */
		private class ReviewContentView extends LinearLayout {
			
			private ReviewContentView(Context context) {
				super(context);
				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);		
				params.weight = 1.0f;
				this.setLayoutParams(params);
				this.setOrientation(LinearLayout.VERTICAL);
			}
		}
		
		/**
		 * Class that holds the "delete" button at the bottom of the single-item review UI.
		 * 
		 * @author mstevens, benelliott
		 */
		private class ReviewButtonView extends MediaButtonView
		{

			private ReviewButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
						// only one button: discard
						onDiscard();
						if (field.getMax() <= 1) {
							// single item review
							removeMedia(field.getLastAttachment(controller.getFileStorageProvider(), controller.getCurrentRecord())); // captures are now always attached, so must be deleted regardless of approval
						} else {
							// reviewing from gallery, so delete
							toReview.delete();
							maxReached = false;  // have now deleted media, so cannot have reached max
							controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY);
						}
						// an item has been deleted, so want the gallery to be refreshed:
						mediaItemsChanged = true; 
						// either go back to capture, or go back to gallery (decided on entry):
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
						// Important: release the click semaphore AFTER the field has been exited
						handlingClick.release();
					}
				};
			}

			@Override
			protected int getNumberOfColumns()
			{
				return 1;
			}

			@Override
			protected void addButtons()
			{
				// Discard button:
				Item discardButton = null;
				File discardImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getDiscardButtonImageRelativePath());
				if(FileHelpers.isReadableFile(discardImgFile))
					discardButton = new FileImageItem(discardImgFile);
				else
					discardButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_trash_svg);
				discardButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(discardButton);
			}
		}
		
	}
	
	/**
	 * LinearLayout that holds the gallery UI (only used when there are multiple media attachments to show).
	 * 
	 * @author benelliott
	 *
	 */
	private class GalleryView extends LinearLayout {
		
		private GalleryPicker pickerView;
		private GalleryButtonView buttonView;
		
		private GalleryView(Context context) {
			super(context);
			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);
			// add gallery picker:
			pickerView = new GalleryPicker(context);
			this.addView(pickerView);
			// add capture button:
			buttonView = new GalleryButtonView(context);
			this.addView(buttonView);
		}
		
		/**
		 * Refreshes the contents of the gallery and the "capture" button.
		 */
		private void refresh() {
			pickerView.loadMedia();
			buttonView.refreshButtons();
		}
		
		/**
		 * A PickerView which allows for the review of multiple media items, such as
		 * images or audio.
		 * @author benelliott
		 *
		 */
		private class GalleryPicker extends PickerView {

			private static final int NUM_COLUMNS = 3; //TODO make configurable?
			private static final int NUM_ROWS = 3;

			private int buttonPadding;

			private GalleryPicker(Context context) {
				super(context);
				
				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
				params.weight = 1.0f;
				this.setLayoutParams(params);

				// Number of columns:
				setNumColumns(NUM_COLUMNS);
				// Item size & padding:
				setItemDimensionsPx(
						collectorUI.getFieldUIPartWidthPx(NUM_COLUMNS),
						collectorUI.getFieldUIPartHeightPx(NUM_ROWS));

				this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
				// Item spacing:
				int spacingPx = collectorUI.getSpacingPx();
				setHorizontalSpacing(spacingPx);
				setVerticalSpacing(spacingPx);

				// When a gallery item is clicked, present it in full screen for review/deletion:
				setOnItemClickListener(new OnItemClickListener() {
					@Override
					public void onItemClick(AdapterView<?> parent, View view,
							int position, long id) {
						controller.getCurrentFieldArguments().put(REVIEW_FILE_PATH_KEY, ((FileItem)getAdapter().getItem(position)).getFile().getAbsolutePath());
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
					}	
				});
			}

			/**
			 * Load the media items into the adapter and then refresh the View.
			 */
			private void loadMedia() {
				PickerAdapter adapter;

				if (mediaItemsChanged) {
					// if an item has been added or deleted, then do a complete rescan for files
					Log.d(TAG,"Media items changed, updating gallery...");
					adapter = new PickerAdapter();
					for (Item item : getMediaItems(controller.getFileStorageProvider(),controller.getCurrentRecord())) {
						item.setPaddingPx(buttonPadding);
						adapter.addItem(item);
					}
					if (adapter.getCount() >= field.getMax()) {
						maxReached = true;
					}
					
					mediaItemsChanged = false;
				}
				else
					// else just use the existing adapter
					adapter = getAdapter();

				// force the picker to update its views
				setAdapter(adapter);
			}
		}

		/**
		 * Class that holds the "capture more" button at the bottom of the gallery UI.
		 * 
		 * @author benelliott
		 *
		 */
		private class GalleryButtonView extends MediaButtonView
		{
			private Item captureMoreButton;

			private GalleryButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
						// "capture more" button clicked, return to camera interface
						if (!maxReached) {
							// add the "go to capture" argument to the field:
							controller.getCurrentFieldArguments().put(GO_TO_CAPTURE_KEY, "true");
							// re-enter current field:
							controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
						}
						// Important: release the click semaphore AFTER the field has been exited
						handlingClick.release();
					}
				};
			}

			/**
			 * Creates a "capture more" button to be used in the picker, but makes it appear greyed out
			 * if the field's maximum number of attachments has been reached.
			 * 
			 * @return the "capture more" button to be added to the UI.
			 */
			private Item createCaptureMoreButton() {
				// creates a "normal" capture button and then disables it if max reached.

				ImageItem captureButton = generateCaptureButton(getContext());

				if (maxReached) {
					// make button look unclickable
					LayeredItem layeredItem = new LayeredItem();
					layeredItem.addLayer(captureButton, false);

					// Make background of layered stack gray:
					layeredItem.setBackgroundColor(CollectorView.COLOR_GRAY);
					// Add grayed-out layer:
					Item grayOutOverlay = new EmptyItem();
					grayOutOverlay.setBackgroundColor(CollectorView.COLOR_MOSTLY_OPAQUE_GRAY);
					layeredItem.addLayer(grayOutOverlay, false);	

					return layeredItem;
				}
				else {
					return captureButton;
				}
			}

			@Override
			protected int getNumberOfColumns()
			{
				return 1;
			}

			@Override
			protected void addButtons()
			{
				// Capture button:
				captureMoreButton = createCaptureMoreButton();
				addButton(captureMoreButton);
			}
		}
		
	}
}
