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
import java.util.Currency;
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
import android.widget.ViewFlipper;

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

	private MediaFlipper mediaFlipper;
	Semaphore handlingClick;
	private boolean goToCapture = false; // flag used to jump straight to capture from gallery 
	boolean multipleCapturesAllowed; // whether or not multiple pieces of media can be associated with this field
	private boolean maxReached; // whether or not the maximum number of pieces of media have been captured for this field
	File captureFile; // file that holds the most recently captured media
	
	public AndroidMediaUI(MF field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		multipleCapturesAllowed = (field.getMax() > 1);	
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
		else
		{
			if(mediaFlipper == null)
				mediaFlipper = new MediaFlipper(collectorUI.getContext());
			
			handlingClick = new Semaphore(1); // (re)initialise semaphore when re-entering field

			if (field.getCount(controller.getCurrentRecord()) == 0 || goToCapture) {
				// if no media or just came from review then go to capture UI
				mediaFlipper.showCaptureLayout();
			}
			else {
				// else go to gallery UI, or review UI if max = 1
				if (field.getMax() == 1) {
					// switch to single review layout:
					mediaFlipper.showReviewLayout();
					// populate with most recent capture:
					populateReviewLayout((ViewGroup)mediaFlipper.findViewById(R.id.review_layout_content),field.getLastAttachment(controller.getFileStorageProvider(), record));
				}
				else
					mediaFlipper.showGalleryLayout();
			}
			return mediaFlipper;
		}
	}

	@Override
	protected void cancel()
	{
		mediaFlipper = null;
		if (captureFile != null) {
			// last capture has been implicitly discarded. TODO: this is a little unsafe,
			// as forgetting to nullify the new capture would cause its deletion
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
	void releaseClick() {
		handlingClick.release();
	}

	
	Context getContext() {
		return (mediaFlipper == null) ? null : mediaFlipper.getContext();
	}
	
	void maximiseCaptureButton() {
		mediaFlipper.maximiseCaptureButton();
	}
	
	void minimiseCaptureButton() {
		mediaFlipper.minimiseCaptureButton();
	}
	
	/**
	 * What to do when the capture button has been pressed.
	 */
	abstract void onCapture();
	
	/**
	 * What to do when a piece of media is discarded after review (e.g. release media player resources).
	 * @return
	 */
	abstract void onDiscard();
	
	/**
	 * 
	 * @return a list of the media items captured in this field.
	 */
	abstract List<Item> getMediaItems(FileStorageProvider fileStorageProvider, Record record);
	
	/**
	 * Generate a "capture" button (may vary by media, e.g. microphone
	 * for audio and camera for photo).
	 * @param context
	 * @return an ImageItem housing an appropriate "capture" button for the media.
	 */
	abstract ImageItem generateCaptureButton(Context context);
	
	/**
	 * Populate a container with views as appropriate to create an interface
	 * for media capture (e.g. viewfinder for camera).
	 * @param captureLayout - the container to populate.
	 */
	abstract void populateCaptureLayout(ViewGroup captureLayout);

	/**
	 * Populate a container with views as appropriate to create an interface for 
	 * the review and deletion of media (e.g. full-page photo).
	 * @param deleteLayout - the container to populate.
	 * @param mediaFile - the media being considered for review.
	 */
	abstract void populateReviewLayout(ViewGroup reviewLayout, File mediaFile);
	
	/**
	 * Flipper that holds two or three views depending on the maximum number of attachments of the media field.
	 * <br>
	 * <br>
	 * If max == 1, flipper holds:
	 * <br>
	 * (1) Capture UI <---> (2) Single-item Review UI
	 * <br>
	 * Else (max > 1), flipper holds:
	 * <br>
	 * (1) Capture UI <---> (2) Single-item Review UI <---> (3) Gallery UI
	 * <br>
	 * If the showNext()/showPrevious() calls are unclear, refer back to this!
	 * <br>
	 * <br>
	 * Note: If the Gallery UI is enabled, then captures will skip (2) when they have just been made.
	 * 
	 * @author benelliott
	 *
	 */
	private class MediaFlipper extends ViewFlipper {
			
		private LinearLayout captureLayoutContainer;
		private LinearLayout reviewLayoutContainer;
		private LinearLayout galleryLayoutContainer;
		private MediaGalleryView gallery;
		
		private LinearLayout galleryLayoutButtonContainer;
		private LinearLayout captureLayoutButtonContainer;
		private android.view.ViewGroup.LayoutParams originalCaptureButtonParams;
		private boolean captureButtonMaximised = false;
		
		public MediaFlipper(Context context) {
			super(context);
			
			// --- Capture UI:
			captureLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_capture, this, false);
			// Add the Capture button:
			captureLayoutButtonContainer = (LinearLayout) captureLayoutContainer.findViewById(R.id.capture_layout_buttons);
			originalCaptureButtonParams = captureLayoutButtonContainer.getLayoutParams();
			captureLayoutButtonContainer.addView(new CaptureButtonView(context));
			// Add the CaptureLayout to the screen
			this.addView(captureLayoutContainer);
			
			// --- Review UI:
			reviewLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_review, this, false);
			// Add the confirm/cancel buttons:
			final LinearLayout reviewLayoutButtons = (LinearLayout) reviewLayoutContainer.findViewById(R.id.review_layout_buttons);
			reviewLayoutButtons.addView(new ReviewButtonView(getContext()));
			// Add the ReviewLayout to the screen
			this.addView(reviewLayoutContainer);
			
			// --- Gallery UI:
			if (multipleCapturesAllowed) {
				// Multiple pieces of media can be taken, so use a PickerView for review after confirmation:
				galleryLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_picker, this, false);
				// Add the confirm/cancel buttons:
				galleryLayoutButtonContainer = (LinearLayout) galleryLayoutContainer.findViewById(R.id.picker_layout_buttons);
				galleryLayoutButtonContainer.addView(new GalleryButtonView(context));
				final LinearLayout pickerViewContainer = (LinearLayout) galleryLayoutContainer.findViewById(R.id.picker_layout_picker_container);
				gallery = new MediaGalleryView(context);
				pickerViewContainer.addView(gallery);
				// Add the picker:
				this.addView(galleryLayoutContainer);
			}
		}
		
		/**
		 * Execute the capture operation (may be called on a button press, or to
		 * "force" a capture automatically).
		 */
        private void performCapture() {
			goToCapture = false; //just made a capture, so go to gallery instead (unless multiple disabled)
			onCapture();
			refreshCaptureButton(); // TODO if performance issues, may not want to do this on every press
		}

        /**
         * Recreate the gallery buttons in case they should be changed (e.g. if 
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
		 * Force the MediaFlipper to show the capture UI, and populate it as specified
		 * by the subclass.
		 */
		private void showCaptureLayout() {
			if (getCurrentView() == reviewLayoutContainer) {
				showPrevious();
			}
			if (getCurrentView() == galleryLayoutContainer) {
				showNext();
			}
			populateCaptureLayout((ViewGroup)((ViewGroup)this.getCurrentView()).findViewById(R.id.capture_layout_content));
		}
		
		/**
		 * Force the MediaFlipper to show the single-item review UI. Not automatically populated  
		 * since the UI may be populated with information about a specific item, which cannot be 
		 * predicted here.
		 */
		private void showReviewLayout() {
			if (multipleCapturesAllowed && field.getCount(controller.getCurrentRecord()) > 0) {
				controller.addCurrentFieldToHistory();
			}
			if (getCurrentView() == reviewLayoutContainer) {
				return;
			}
			if (getCurrentView() == captureLayoutContainer) {
				showNext();
				return;
			}
			if (getCurrentView() == galleryLayoutContainer) {
				showPrevious();
			}
		}
		
		/**
		 *  Force the MediaFlipper to show the gallery UI; reload its contents and its associated
		 *  buttons.
		 */
		private void showGalleryLayout() {
			if (getCurrentView() == captureLayoutContainer) {
				showPrevious();
			}
			if (getCurrentView() == reviewLayoutContainer) {
				showNext();
			}
			
			gallery.loadMedia();
			refreshGalleryButtons();
		}
		
		private void minimiseCaptureButton() {
			if (captureButtonMaximised) {
				captureButtonMaximised = false;
				captureLayoutContainer.findViewById(R.id.capture_layout_content).setVisibility(View.VISIBLE);
				captureLayoutButtonContainer.setLayoutParams(originalCaptureButtonParams); // TODO remove xml completely
				
				refreshCaptureButton();
			}
		}
		
		private void maximiseCaptureButton() {
			if (!captureButtonMaximised) {
				captureButtonMaximised = true;
				captureLayoutContainer.findViewById(R.id.capture_layout_content).setVisibility(View.GONE);
				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
				captureLayoutButtonContainer.setLayoutParams(params);
				
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
				
				int spacingPx = collectorUI.getSpacingPx();
				setHorizontalSpacing(spacingPx);
				setVerticalSpacing(spacingPx);

				setOnItemClickListener(new OnItemClickListener() {
					@Override
					public void onItemClick(AdapterView<?> parent, View view,
							int position, long id) {
						itemPosition = position;
						// a piece of media has been clicked, so show it and offer deletion
						populateReviewLayout((LinearLayout)reviewLayoutContainer.findViewById(R.id.review_layout_content), ((FileItem)getAdapter().getItem(position)).getFile());
						// Show the view:
						showReviewLayout();
					}	
				});
			}

			/**
			 * Load the media items into the adapter and then refresh the View.
			 */
			private void loadMedia() {
				PickerAdapter adapter = new PickerAdapter();
				for (Item item : getMediaItems(controller.getFileStorageProvider(),controller.getCurrentRecord())) {
					item.setPaddingPx(buttonPadding);
					adapter.addItem(item);
				}
				setAdapter(adapter);
				if (adapter.getCount() >= field.getMax()) {
					maxReached = true;
					refreshGalleryButtons();
				}
				getAdapter().notifyDataSetChanged();
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
			int position;

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
							MediaButtonView.this.position = position;
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

				// Columns
				setNumColumns(getNumberOfColumns()); //TODO necessary?

				// Images/buttons:

				// Button size, padding & background colour:
				
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
						performCapture();
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
							if (!multipleCapturesAllowed) {
								// single item review
								removeMedia(field.getLastAttachment(controller.getFileStorageProvider(), controller.getCurrentRecord())); // captures are now always attached, so must be deleted regardless of approval
							} else {
								// reviewing from gallery, so delete
								gallery.deleteCurrentItem();
								maxReached = false;  // have now deleted media, so cannot have reached max
								goToCapture = false;
							}
							// either go back to capture, or go back to gallery (decided on entry):
							controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
						 // reset last capture file
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
		 * Class that holds the "plus" and "approve" buttons at the bottom of the gallery UI.
		 * 
		 * @author benelliott
		 *
		 */
		private class GalleryButtonView extends MediaButtonView
		{

			private Item plusButton;

			public GalleryButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
							// plus button clicked, return to camera interface
							if (!maxReached) {
								goToCapture = true;
								controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
							}
						handlingClick.release();
					}
				};
			}

			/**
			 * Creates a "plus" button to be used in the picker, but makes it appear greyed out
			 * if the field's maximum number of attachments has been reached.
			 * 
			 * @return the "plus" button to be added to the UI.
			 */
			private Item createPlusButton() {
				// creates a "normal" plus button and then disables it if max reached.
				Item plusButton = null;
				File plusImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getPlusButtonImageRelativePath());
				if(FileHelpers.isReadableFile(plusImgFile))
					plusButton = new FileImageItem(plusImgFile);
				else
					plusButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_plus_svg);
				plusButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));

				if (maxReached) {
					// make button look unclickable
					LayeredItem layeredItem = new LayeredItem();
					layeredItem.addLayer(plusButton, false);

					// Make background of layered stack gray:
					layeredItem.setBackgroundColor(CollectorView.COLOR_GRAY);
					// Add grayed-out layer:
					Item grayOutOverlay = new EmptyItem();
					grayOutOverlay.setBackgroundColor(CollectorView.COLOR_MOSTLY_OPAQUE_GRAY);
					layeredItem.addLayer(grayOutOverlay, false);	

					return layeredItem;
				}
				else {
					return plusButton;
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
				plusButton = createPlusButton();
				addButton(plusButton);
			}
		}		
	}
}
