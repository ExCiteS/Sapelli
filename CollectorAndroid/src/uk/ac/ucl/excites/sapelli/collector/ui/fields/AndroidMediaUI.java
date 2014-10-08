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
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ViewFlipper;

/**
 * Built-in camera view
 * 
 * TODO added photo/no photo buttons before entering actual camera mode OR going to naive camera app (which is no longer called from the controller!)
 * 
 * TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time (on Android v2.x only?)
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public abstract class AndroidMediaUI<MF extends MediaField> extends MediaUI<MF, View, CollectorView>
{

	private MediaFlipper mediaFlipper;
	private Semaphore handlingClick = new Semaphore(1);
	private boolean goToCapture = false; // flag used to jump straight to capture from gallery 
	boolean multipleCapturesAllowed; // whether or not multiple pieces of media can be associated with this field
	private boolean maxReached; // whether or not the maximum number of pieces of media have been captured for this field
	File lastCaptureFile; // file that holds the most recently captured media

	public AndroidMediaUI(MF field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		multipleCapturesAllowed = (field.getMax() > 1);	
		maxReached = (field.getCount(controller.getCurrentRecord()) >= field.getMax());
	}
	
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

			if (field.getCount(controller.getCurrentRecord()) == 0 || !multipleCapturesAllowed || goToCapture) {
				// if no media, not multiple, or just came from gallery then go to capture UI
				mediaFlipper.showCaptureLayout();
				onInitialiseCaptureMode();
			}
			else {
				// else go to gallery (e.g. if have just captured media / if skipping back)
				mediaFlipper.showGalleryLayout();
				mediaFlipper.updateGallery();
			}
			return mediaFlipper;
		}
	}
	
	void showCaptureForReview() {
		mediaFlipper.showCaptureForReview();
	}
	
	void attachMediaFile() {
		try {
			if (multipleCapturesAllowed) {
				mediaAddedButNotDone(lastCaptureFile);										
			} else {
				mediaDone(lastCaptureFile, true);
				finalise(); 
			}
		} catch (Exception e) {
			mediaDone(null, true);
			e.printStackTrace();
			finalise();
		}
	}
	
	Context getContext() {
		return (mediaFlipper == null) ? null : mediaFlipper.getContext();
	}
	
	@Override
	protected void cancel()
	{
		if(mediaFlipper != null)
		{
			finalise();
			mediaFlipper = null;
		}
	}

	/**
	 * What to do when entering capture mode (e.g. start camera viewfinder).
	 */
	abstract void onInitialiseCaptureMode();
	
	/**
	 * What to do when the capture button has been pressed.
	 * @return a boolean indicating whether or not to process
	 * other click events as soon as this method returns (i.e. whether the task
	 * is finished immediately or whether we must wait for some event before
	 * interaction can continue - such as a picture callback).
	 */
	abstract boolean onCapture();
		
	/**
	 * What to do on exit of this field (e.g. release resources).
	 */
	abstract void finalise();
	
	/**
	 * 
	 * @return a list of the media items captured in this field.
	 */
	abstract List<Item> getMediaItems();
	
	/**
	 * Generate a "capture" button (may vary by media, e.g. microphone
	 * for audio and camera for photo).
	 * @param context
	 * @return an ImageItem housing an appropriate "capture" button for the media.
	 */
	abstract ImageItem getCaptureButton(Context context);
	
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
	 * If max == 1, flipper holds:
	 * <br>
	 * (1) Capture UI <---> (2) Review/Discard UI
	 * <br>
	 * Else (max > 1), flipper holds:
	 * <br>
	 * (1) Capture UI <---> (2) Review/Delete UI <---> (3) Gallery UI
	 * <br>
	 * If the showNext()/showPrevious() calls are unclear, refer back to this!
	 * 
	 * @author Ben
	 *
	 */
	private class MediaFlipper extends ViewFlipper {
			
		private LinearLayout captureLayoutContainer;
		private LinearLayout reviewLayoutContainer;
		private LinearLayout galleryLayoutContainer;
		private MediaGalleryView gallery;
		
		private LinearLayout galleryLayoutButtonContainer;
		
		public MediaFlipper(Context context) {
			super(context);
			
			// --- Capture UI:
			captureLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_capture, this, false);
			LinearLayout captureLayout = (LinearLayout) captureLayoutContainer.getChildAt(0);
			populateCaptureLayout(captureLayout);
			// Add the Capture button:
			final LinearLayout captureLayoutButtons = (LinearLayout) captureLayoutContainer.findViewById(R.id.capture_layout_buttons);
			captureLayoutButtons.addView(new CaptureButtonView(context));
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
				refreshGalleryButtons();
				gallery = new MediaGalleryView(context);
				pickerViewContainer.addView(gallery);
				gallery.loadMedia();
				// Add the picker:
				this.addView(galleryLayoutContainer);
			}
		}
		
		private void updateGallery() {
			gallery.loadMedia();	        
		}
		
		private void showCaptureForReview() {
			// TODO don't run on UI thread?
			// data has been received from capture, so present it for review
			if (multipleCapturesAllowed) {
				attachMediaFile(); // implicitly approve media so it can be reviewed in gallery
				updateGallery(); // refresh gallery to show the new item
			}
			else {
				showNext(); // just go to simple review UI
				populateReviewLayout((LinearLayout)reviewLayoutContainer.getChildAt(0), lastCaptureFile);
			}
			handlingClick.release();
		}

		private void refreshGalleryButtons() {
			galleryLayoutButtonContainer.removeAllViews();
			galleryLayoutButtonContainer.addView(new GalleryButtonView(getContext()));
        }
		
		private void showCaptureLayout() {
			if (getCurrentView() == captureLayoutContainer) {
				return;
			}
			if (getCurrentView() == reviewLayoutContainer) {
				showPrevious();
				return;
			}
			if (getCurrentView() == galleryLayoutContainer) {
				showNext();
			}
		}
		
		private void showGalleryLayout() {
			if (getCurrentView() == galleryLayoutContainer) {
				return;
			}
			if (getCurrentView() == captureLayoutContainer) {
				showPrevious();
				return;
			}
			if (getCurrentView() == reviewLayoutContainer) {
				showNext();
			}
		}
		
		private class MediaGalleryView extends PickerView {

			private static final int NUM_COLUMNS = 3; //TODO make configurable?
			private static final int NUM_ROWS = 3;

			private int itemPosition;

			public MediaGalleryView(Context context) {
				super(context);

				// Number of columns:
				setNumColumns(NUM_COLUMNS);
				// Item size & padding:
				setItemDimensionsPx(
						collectorUI.getFieldUIPartWidthPx(NUM_COLUMNS),
						collectorUI.getFieldUIPartHeightPx(NUM_ROWS));

				setOnItemClickListener(new OnItemClickListener() {
					@Override
					public void onItemClick(AdapterView<?> parent, View view,
							int position, long id) {
						// a piece of media has been clicked, so show it and offer deletion
						populateReviewLayout((LinearLayout)reviewLayoutContainer.getChildAt(0), ((FileItem)getAdapter().getItem(position)).getFile());
						// Show the view:
						showPrevious(); // currently in gallery, so go backwards
					}	
				});
			}

			private void loadMedia() {
				PickerAdapter adapter = new PickerAdapter();
				for (Item item : getMediaItems()) {
					adapter.addItem(item);
				}
				setAdapter(adapter);
				if (adapter.getCount() >= field.getMax()) {
					maxReached = true;
					refreshGalleryButtons();
				}
				getAdapter().notifyDataSetChanged();
			}

			private void deleteCurrentMedia() {
				Item currentMediaItem = getAdapter().getItem(itemPosition);
				File currentMediaFile = ((FileItem)currentMediaItem).getFile();
				removeMedia(currentMediaFile);
				getAdapter().removeItem(currentMediaItem);
				maxReached = false;  // have now deleted media, so cannot have reached max
				refreshGalleryButtons();
				getAdapter().notifyDataSetChanged();
			}
		}
		
		/**
		 * @author mstevens
		 */
		private abstract class MediaButtonView extends PickerView
		{
			private int buttonPadding;
			private int buttonBackColor;
			Runnable buttonAction;
			int position;
			/**
			 * @param context
			 */
			public MediaButtonView(Context context)
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
				setPadding(0, collectorUI.getSpacingPx(), 0, 0);

				// Columns
				setNumColumns(getNumberOfColumns()); //TODO necessary?

				// Images/buttons:

				// Button size, padding & background colour:
				this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
				this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP * 3);
				this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);

				// The addButtons() should be called after the button parameters (size, padding etc.) have been setup
				addButtons();

				// And finally:
				setAdapter(getAdapter()); // this is supposedly needed on Android v2.3.x (TODO test it)
			}

			protected void addButton(Item button)
			{
				button.setPaddingPx(buttonPadding);
				button.setBackgroundColor(buttonBackColor);
				getAdapter().addItem(button);
			}

			protected abstract int getNumberOfColumns();

			protected abstract void addButtons();

		}
	
		private class CaptureButtonView extends MediaButtonView
		{

			public CaptureButtonView(Context context)
			{
				super(context);

				buttonAction = new Runnable() {
					@Override
					public void run() {
						goToCapture = false; //just made a capture, so go to gallery instead (unless multiple disabled)
						if (onCapture()) {
							// if returns true, allow other clicks to occur after onCapture returned
							handlingClick.release();
						}
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
				addButton(getCaptureButton(this.getContext()));
			}
		}
		
		/**
		 * @author mstevens
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
						//there are 2 buttons: approve (pos=0) & discard (pos=1)
						if (position == 0) // media approved
							if (!multipleCapturesAllowed) {
								// only reviewing the most recent photo, so save it
								attachMediaFile();
							}
							else {
								// looking at photo after selecting from gallery, so just return to gallery
								showNext();
							}
						else 
						{ // position == 1, media discarded / deleted
							if (!multipleCapturesAllowed) {
								// reviewing after capture, so discard
								//onDiscard(); TODO: with current method for saving files, temp files eventually discarded automatically.
								showPrevious(); //switch back to capture mode
							}
							else {
								// reviewing from gallery, so delete
								gallery.deleteCurrentMedia();
								if (field.getCount(controller.getCurrentRecord()) == 0) {
									// no media left, so go straight back to capture
									controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
								}
								else {
									showNext(); //go back to gallery
								}
							}
						}
						 // reset last capture file
						lastCaptureFile = null;
						handlingClick.release();
					}
				};
			}

			@Override
			protected int getNumberOfColumns()
			{
				return 2;
			}

			@Override
			protected void addButtons()
			{
				// Approve button:
				Item approveButton = null;
				File approveImgFile = controller.getProject().getImageFile(field.getApproveButtonImageRelativePath());
				if(FileHelpers.isReadableFile(approveImgFile))
					approveButton = new FileImageItem(approveImgFile);
				else
					approveButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_tick_svg);
				approveButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(approveButton);

				// Discard button:
				Item discardButton = null;
				File discardImgFile = controller.getProject().getImageFile(field.getDiscardButtonImageRelativePath());
				if(FileHelpers.isReadableFile(discardImgFile))
					discardButton = new FileImageItem(discardImgFile);
				else
					discardButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_trash_svg);
				discardButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(discardButton);
			}
		}
	
		private class GalleryButtonView extends MediaButtonView
		{

			private Item captureButton;
			private Item approveButton;

			public GalleryButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
						if (position == 0) {
							// camera button clicked, return to camera interface
							if (!maxReached) {
								goToCapture = true;
								controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
							}
						} else {
							//approve button clicked, so proceed to next field
							goToCapture = false;
							mediaDone(null, true);
						}
						handlingClick.release();
					}
				};
			}

			private Item createCaptureButton() {
				// gets a "normal" capture button and then disables it if max reached. TODO: change to "add"
				Item captureButton = getCaptureButton(this.getContext());
				captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				if (maxReached) {
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
				return 2;
			}

			@Override
			protected void addButtons()
			{
				// Capture button:
				captureButton = createCaptureButton();
				addButton(captureButton);

				// Approve button:
				approveButton = null;
				File approveImgFile = controller.getProject().getImageFile(field.getApproveButtonImageRelativePath());
				if(FileHelpers.isReadableFile(approveImgFile))
					approveButton = new FileImageItem(approveImgFile);
				else
					approveButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_tick_svg);
				approveButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(approveButton);
			}
		}		
	}
}
