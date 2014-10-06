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
import java.io.FileOutputStream;
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
import android.widget.ViewSwitcher;

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

	private CaptureView captureView;
	private GalleryView galleryView;

	private Semaphore handlingClick = new Semaphore(1);
	private boolean goToCapture = false; // flag used to jump straight to capture from gallery 
	protected byte[] capturedMediaData;

	public AndroidMediaUI(MF field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	protected void cancel()
	{
		if(captureView != null)
		{
			finalise();
			captureView = null;
		}
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
			if(captureView == null)
				captureView = new CaptureView(collectorUI.getContext());
			captureView.update();

			if (field.getCount(controller.getCurrentRecord()) == 0 || !field.isMultiple() || goToCapture) {
				// if no media, not multiple, or just came from gallery then go to capture UI
				return captureView;
			}
			else {
				// else go to gallery (e.g. if have just captured media / if skipping back)
				if (galleryView == null) {
					galleryView = new GalleryView(collectorUI.getContext());
				}
				galleryView.update();
				return galleryView;
			}
		}
	}
	
	protected void dataReceived() {
		captureView.dataReceived();
	}
	
	protected Context getContext() {
		return (captureView == null) ? null : captureView.getContext();
	}
	
	
	public abstract boolean onCapture();
	
	public abstract void onMediaSaved();
	
	public abstract void onCaptureStarted();
	
	public abstract void populateCaptureLayout(ViewGroup captureLayout);
	
	public abstract void populateReviewLayout(ViewGroup reviewLayout);

	public abstract void populateDeleteLayout(ViewGroup deleteLayout, File mediaFile);
	
	public abstract void finalise();
		
	public abstract List<Item> getMediaItems();
	
	protected abstract ImageItem getCaptureButton(Context context);

		
	private class CaptureView extends ViewSwitcher
	{

		static private final String TAG = "CaptureView";

		// UI elements:
		private LinearLayout captureLayoutContainer;
		private LinearLayout reviewLayoutContainer; // Layout for reviewing single items
				
		public CaptureView(Context context)
		{
			super(context);
			// --- Capture UI:
			captureLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_capture, this, false);
			LinearLayout captureLayout = (LinearLayout) captureLayoutContainer.getChildAt(0);
			AndroidMediaUI.this.populateCaptureLayout(captureLayout);

			// Add the Capture button:
			final LinearLayout captureLayoutButtons = (LinearLayout) captureLayoutContainer.findViewById(R.id.capture_layout_buttons);
			captureLayoutButtons.addView(new CaptureButtonView(context));

			// Add the CaptureLayout to the screen
			this.addView(captureLayoutContainer);

			// --- Review UI:
			reviewLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_review, this, false);
			LinearLayout reviewLayout = (LinearLayout) reviewLayoutContainer.getChildAt(0);
			AndroidMediaUI.this.populateReviewLayout(reviewLayout);

			// Add the confirm/cancel buttons:
			final LinearLayout reviewLayoutButtons = (LinearLayout) reviewLayoutContainer.findViewById(R.id.review_layout_buttons);
			reviewLayoutButtons.addView(new ReviewButtonView(getContext()));

			// Add the ReviewLayout to the screen
			this.addView(reviewLayoutContainer);
		}

		public void update()
		{
			// switch back to capture UI if necessary:
			if (getCurrentView() == reviewLayoutContainer)
				showNext();
		}
		
		private void dataReceived() {
			showNext();
			handlingClick.release();
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
						if (AndroidMediaUI.this.onCapture()) 
							// if returns true, a capture has been made (rather than just started)
							dataReceived();
						else
							handlingClick.release();
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
				addButton(AndroidMediaUI.this.getCaptureButton(this.getContext()));
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
						if (position == 0) { // media approved
							try { // Save media to file:
								File mediaFile =
										field.getNewTempFile(controller
												.getCurrentRecord());
								FileOutputStream fos =
										new FileOutputStream(mediaFile);
								fos.write(capturedMediaData);
								fos.close();
								if (field.isMultiple()) {
									mediaAddedButNotDone(mediaFile);										
								} else {
									mediaDone(mediaFile, true);
									AndroidMediaUI.this.onMediaSaved();
								}

							} catch (Exception e) {
								mediaDone(null, true);
								AndroidMediaUI.this.onMediaSaved();
							}
						} else 
						{ // position == 1, media discarded
							showNext(); // switch back to capture mode
							AndroidMediaUI.this.onCaptureStarted();
						}
						capturedMediaData = null;
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
	}
	
	private class GalleryView extends ViewSwitcher {
		
		private Context context;

		private LinearLayout pickerLayoutContainer;
		private MediaPickerView mediaPicker;
		private LinearLayout deleteLayoutContainer;
		private LinearLayout deleteLayout;
		private LinearLayout pickerLayoutButtons;
		
		private boolean maxReached = false; // whether or not max media taken

		public GalleryView(Context context) {
			super(context);
			this.context = context;

			// Multiple pieces of media can be taken, so use a PickerView for review after confirmation:
			pickerLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_picker, this, false);

			// Add the confirm/cancel buttons:
			pickerLayoutButtons = (LinearLayout) pickerLayoutContainer.findViewById(R.id.picker_layout_buttons);
			pickerLayoutButtons.addView(new PickerButtonView(context));
			final LinearLayout pickerViewContainer = (LinearLayout) pickerLayoutContainer.findViewById(R.id.picker_layout_picker_container);
			refreshPickerButtons();
			mediaPicker = new MediaPickerView(context);
			pickerViewContainer.addView(mediaPicker);
			mediaPicker.loadMedia();
			// Add the picker:
			this.addView(pickerLayoutContainer);

			// Add the layout for media review/deletion after capture:
			deleteLayoutContainer = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_media_review, this, false);

			deleteLayout = (LinearLayout) deleteLayoutContainer.getChildAt(0);
			// Add the confirm/cancel buttons:
			final LinearLayout deleteLayoutButtons = (LinearLayout) deleteLayoutContainer.findViewById(R.id.review_layout_buttons);
			deleteLayoutButtons.addView(new DeleteButtonView(getContext()));

			// Add the view:
			addView(deleteLayoutContainer);
		}

		public void update() {
			mediaPicker.loadMedia();	        
		}

		public void refreshPickerButtons() {
			pickerLayoutButtons.removeAllViews();
			pickerLayoutButtons.addView(new PickerButtonView(context));
        }

		private void showMediaDeleteLayout(File mediaFile) {
			AndroidMediaUI.this.populateDeleteLayout(deleteLayout, mediaFile);
			// Show the view:
			showNext();
		}

		private class MediaPickerView extends PickerView {

			private static final int NUM_COLUMNS = 3; //TODO make configurable?
			private static final int NUM_ROWS = 3;

			private int itemPosition;

			public MediaPickerView(Context context) {
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
						showMediaDeleteLayout(((FileItem)getAdapter().getItem(position)).getFile());
					}	
				});
			}

			protected void loadMedia() {
				PickerAdapter adapter = new PickerAdapter();
				for (Item item : AndroidMediaUI.this.getMediaItems()) {
					adapter.addItem(item);
				}
				setAdapter(adapter);
				if (adapter.getCount() >= field.getMax()) {
					maxReached = true;
					GalleryView.this.refreshPickerButtons();
				}
				getAdapter().notifyDataSetChanged();
			}

			protected void deleteCurrentMedia() {
				Item currentMediaItem = getAdapter().getItem(itemPosition);
				File currentMediaFile = ((FileItem)currentMediaItem).getFile();
				removeMedia(currentMediaFile);
				getAdapter().removeItem(currentMediaItem);
				maxReached = false;  // have now deleted media, so cannot have reached max
				GalleryView.this.refreshPickerButtons();
				getAdapter().notifyDataSetChanged();
			}
		}


		/**
		 * @author mstevens, benelliott
		 */
		private class DeleteButtonView extends MediaButtonView
		{

			public DeleteButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
						//there are 2 buttons: approve (pos=0) & delete (pos=1)
						if (position == 0) { 
							// "approve" selected, just return to picker
							showNext();
						} 
						else  { 
							// "delete" selected, delete media
							mediaPicker.deleteCurrentMedia();
							if (field.getCount(controller.getCurrentRecord()) == 0) {
								// no media left, so go back to capture
								controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
							}
							else {
								showNext(); //go back to gallery
							}
						}
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

				// Delete button:
				Item deleteButton = null;
				//				File discardImgFile = controller.getProject().getImageFile(field.getDiscardButtonImageRelativePath());
				//				if(FileHelpers.isReadableFile(discardImgFile))
				//					deleteButton = new FileImageItem(discardImgFile);
				//				else
				deleteButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_trash_svg);
				deleteButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(deleteButton);
			}
		}

		private class PickerButtonView extends MediaButtonView
		{

			private Item captureButton;
			private Item approveButton;

			public PickerButtonView(Context context)
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
								controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE); //TODO ask Matthias
							}
							else {
								// handle user pressing camera button when max reached?
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
				Item captureButton = AndroidMediaUI.this.getCaptureButton(this.getContext());
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
}
