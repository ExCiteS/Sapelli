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
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI;
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
import android.view.LayoutInflater;
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
	private static final String TAG = "AndroidMediaUI";
	private static final String REVIEW_FILE_PATH_KEY = "REVIEW_FILE_PATH"; // key for obtaining review filepath from field arguments
	private static final String GO_TO_CAPTURE_KEY = "GO_TO_CAPTURE";

	protected Semaphore handlingClick;
	private boolean maxReached; // whether or not the maximum number of pieces of media have been captured for this field
	private boolean mediaItemsChanged = false; 	// whether or not the media items have been changed (whether the gallery needs to be redrawn)
	protected File captureFile; // file that holds the most recently captured media
	private MediaGalleryView gallery;

	private LinearLayout captureLayoutContainer;
	private LinearLayout reviewLayoutContainer;
	private LinearLayout galleryLayoutContainer;

	private LinearLayout galleryLayoutButtonContainer;
	private LinearLayout captureLayoutButtonContainer;

	private boolean captureButtonMaximised = false;
	// keep hold of the original minimised capture button params when it is maximised (could tidy this if XML was removed):
	private android.view.ViewGroup.LayoutParams originalCaptureButtonParams;

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
	 * <li> if no media has been captured, or the capture UI has been specifically requested, 
	 * return the capture UI. If returning from the gallery UI and this field specifies that
	 * capturing begins immediately when the "add" button is pressed, then the capture process is 
	 * started.</li>
	 * <li> if there is already captured media to display, and the capture UI has not been requested, 
	 * then return either the single-item review UI (e.g. full-page image) if the field can have at most one
	 * attachment, or the gallery UI if multiple attachments are acceptable.</li>
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

		// check if the field's arguments specify that we go straight to capture:
		boolean goToCapture = Boolean.parseBoolean(controller.getCurrentFieldArguments().getValue(GO_TO_CAPTURE_KEY)); // returns false if not present

		if (field.getCount(controller.getCurrentRecord()) == 0 || goToCapture) {
			// if no media or just came from review then go to capture UI

			if (captureLayoutContainer == null)
				initCaptureLayoutContainer();
			// populate capture layout (e.g. set up photo viewfinder)
			populateCaptureLayout((ViewGroup)captureLayoutContainer.findViewById(R.id.capture_layout_content));
			return captureLayoutContainer;

		} else {
			// else go to single-item review or gallery review

			// check if the field's arguments specify a single file to review:
			String reviewPath = controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY);
			if (field.getMax() == 1 || reviewPath != null) {
				// if file in argument or only one file possible then show single review UI

				if (reviewLayoutContainer == null)
					initReviewLayoutContainer();
				if (reviewPath != null)
					// if arguments specified a file, populate review UI with it
					populateReviewLayout((ViewGroup)reviewLayoutContainer.findViewById(R.id.review_layout_content), new File(reviewPath));
				else
					// else we are here because max == 1, so just use the most recent attachment
					populateReviewLayout((ViewGroup)reviewLayoutContainer.findViewById(R.id.review_layout_content), field.getLastAttachment(controller.getFileStorageProvider(), record));
				// remove the filepath from the field's arguments so we do not re-enter single-item review unintentionally
				controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY); // clear review path in arguments
				return reviewLayoutContainer;

			} else {
				// max > 1 and we have not been supplied with a filepath, so go to gallery

				if (galleryLayoutContainer == null)
					initGalleryLayoutContainer();
				// force the gallery to update its contents and its button:
				gallery.loadMedia();
				refreshGalleryButtons();
				return galleryLayoutContainer;

			}
		}
	}

	private void initCaptureLayoutContainer() {
		captureLayoutContainer = (LinearLayout) LayoutInflater.from(collectorUI.getContext()).inflate(R.layout.collector_media_capture, null, false);
		captureLayoutButtonContainer = (LinearLayout) captureLayoutContainer.findViewById(R.id.capture_layout_buttons);
		originalCaptureButtonParams = captureLayoutButtonContainer.getLayoutParams(); // save XML-defined params for later
		captureLayoutButtonContainer.addView(new CaptureButtonView(collectorUI.getContext()));
	}

	private void initReviewLayoutContainer() {
		// -------------------------- TODO this is the code that alters the control button behaviour - may want to remove it
		if (field.getMax() > 1 && field.getCount(controller.getCurrentRecord()) > 0) {
			// put this field on the stack, so that it is added to the user's history:
			controller.addCurrentFieldToHistory();
			// remove the forward button on review:
			ControlsUI.State[] newStates = {/* back */ ControlsUI.State.SHOWN_ENABLED, /* exit */ ControlsUI.State.SHOWN_ENABLED, /* forward */ ControlsUI.State.HIDDEN};
			collectorUI.getControlsUI().setControlStates(newStates);
		}
		// --------------------------

		reviewLayoutContainer = (LinearLayout) LayoutInflater.from(collectorUI.getContext()).inflate(R.layout.collector_media_review, null, false);
		LinearLayout reviewLayoutButtons = (LinearLayout) reviewLayoutContainer.findViewById(R.id.review_layout_buttons);
		reviewLayoutButtons.addView(new ReviewButtonView(getContext()));
	}

	private void initGalleryLayoutContainer() {
		galleryLayoutContainer = (LinearLayout) LayoutInflater.from(collectorUI.getContext()).inflate(R.layout.collector_media_picker, null, false);
		// Add the confirm/cancel buttons:
		galleryLayoutButtonContainer = (LinearLayout) galleryLayoutContainer.findViewById(R.id.picker_layout_buttons);
		galleryLayoutButtonContainer.addView(new GalleryButtonView(collectorUI.getContext()));
		// Add the gallery itself:
		final LinearLayout pickerViewContainer = (LinearLayout) galleryLayoutContainer.findViewById(R.id.picker_layout_picker_container);
		gallery = new MediaGalleryView(collectorUI.getContext());
		pickerViewContainer.addView(gallery);
	}

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
	 * Release the semaphore that controls click handling. This method is needed because some 
	 * media types use a callback to determine when the media has been acquired (e.g. photos),
	 * so the semaphore cannot be released as soon as the {@link #onCapture()} method completes.
	 */
	protected void releaseClick() {
		handlingClick.release();
	}


	protected Context getContext() {
		return collectorUI.getContext();
	}

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

	/**
	 * Recreate the gallery button in case it should be changed (e.g. if 
	 * the number of attachments has now reached the maximum for this field).
	 */
	private void refreshGalleryButtons() {
		// recreate gallery buttons in case of change
		galleryLayoutButtonContainer.removeAllViews();
		galleryLayoutButtonContainer.addView(new GalleryButtonView(getContext()));
	}

	/**
	 * Recreate the capture button in case it should be changed (e.g. if
	 * recording audio, want capture button to display a "stop" icon once 
	 * recording has started).
	 */
	private void refreshCaptureButton() {
		// recreate capture button in case of change
		captureLayoutButtonContainer.removeAllViews();
		captureLayoutButtonContainer.addView(new CaptureButtonView(getContext()));
	}

	/**
	 * Restores a maximised capture button to its original position at the bottom of the screen.
	 */
	protected void minimiseCaptureButton() {
		if (captureButtonMaximised) {
			captureButtonMaximised = false;
			// show all other content from capture UI:
			captureLayoutContainer.findViewById(R.id.capture_layout_content).setVisibility(View.VISIBLE);
			// restore capture button's original (minimised) layout parameters:
			captureLayoutButtonContainer.setLayoutParams(originalCaptureButtonParams);
			// redraw the capture button:
			refreshCaptureButton();
		}
	}

	/**
	 * Expands a minimised capture button to take up the entire capture UI (e.g. for audio recording).
	 */
	protected void maximiseCaptureButton() {
		if (!captureButtonMaximised) {
			captureButtonMaximised = true;
			// hide all other content from capture UI:
			captureLayoutContainer.findViewById(R.id.capture_layout_content).setVisibility(View.GONE);
			// fill the capture UI with the capture button:
			LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
			captureLayoutButtonContainer.setLayoutParams(params);
			// redraw the capture button:
			refreshCaptureButton();
		}
	}

	/**
	 * A PickerView which allows for the review of multiple media items, such as
	 * images or audio.
	 * @author benelliott
	 *
	 */
	private class MediaGalleryView extends PickerView {

		private static final int NUM_COLUMNS = 3; //TODO make configurable?
		private static final int NUM_ROWS = 3;

		private int buttonPadding;
		private int itemPosition;

		public MediaGalleryView(Context context) {
			super(context);

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
					itemPosition = position;
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
					refreshGalleryButtons();
				}
				mediaItemsChanged = false;
			}
			else
				// else just use the existing adapter
				adapter = getAdapter();

			// force the picker to update its views
			setAdapter(adapter);
		}

		/**
		 * Delete the item that has just been selected for review (as 
		 * represented by itemPosition).
		 */
		private void deleteCurrentItem() {
			File file = ((FileItem)getAdapter().getItem(itemPosition)).getFile();
			removeMedia(file);
		}
	}

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

		public MediaButtonView(Context context) {
			this(context, false);
		}

		public MediaButtonView(Context context, boolean maximised)
		{
			super(context);

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

			// Columns:
			setNumColumns(getNumberOfColumns());

			// Buttons:
			if (maximised) // fill all available space
				this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, collectorUI.getFieldUIHeightPx());
			else {// just set to default height
				setPadding(0, collectorUI.getSpacingPx(), 0, 0);
				this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
			}

			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP * 3);
			this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);

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
			button.setPaddingPx(buttonPadding);
			button.setBackgroundColor(buttonBackColor);
			getAdapter().addItem(button);
		}

		protected abstract int getNumberOfColumns();

		/**
		 * Add all necessary buttons to the picker.
		 */
		protected abstract void addButtons();

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
			super(context, captureButtonMaximised);
			buttonAction = new Runnable() {
				@Override
				public void run() {
					controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY); //just made a capture, so go to gallery instead (unless multiple disabled)
					onCapture();
					refreshCaptureButton(); // TODO if performance issues, may not want to do this on every press
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

	/**
	 * Class that holds the "approve" and "delete" buttons at the bottom
	 * of the single-item review UI.
	 * 
	 * @author mstevens, benelliott
	 */
	private class ReviewButtonView extends MediaButtonView
	{

		public ReviewButtonView(Context context)
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
						gallery.deleteCurrentItem();
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

	/**
	 * Class that holds the "capture more" button at the bottom of the gallery UI.
	 * 
	 * @author benelliott
	 *
	 */
	private class GalleryButtonView extends MediaButtonView
	{

		private Item captureMoreButton;

		public GalleryButtonView(Context context)
		{
			super(context);
			setHorizontalSpacing(collectorUI.getSpacingPx());

			buttonAction = new Runnable() {
				@Override
				public void run() {
					// "capture more" button clicked, return to camera interface
					if (!maxReached) {
						controller.getCurrentFieldArguments().put(GO_TO_CAPTURE_KEY, "true");
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
