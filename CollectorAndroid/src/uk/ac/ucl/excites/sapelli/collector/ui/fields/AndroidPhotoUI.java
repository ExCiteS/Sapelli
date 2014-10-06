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
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.BitmapUtils;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.ProgressDialog;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.hardware.Camera;
import android.hardware.Camera.PictureCallback;
import android.net.Uri;
import android.os.AsyncTask;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
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
public class AndroidPhotoUI extends PhotoUI<View, CollectorView>
{

	private CameraView cameraView;
	private GalleryView galleryView;

	private Semaphore handlingClick = new Semaphore(1);
	private boolean goToCapture = false; // flag used to jump straight to capture from gallery 


	public AndroidPhotoUI(PhotoField field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	protected void cancel()
	{
		if(cameraView != null)
		{
			cameraView.finalise();
			cameraView = null;
		}
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		//TODO take "enabled" into account
		if(onPage)
		{
			//TODO photo-on-page button
			return null;
		}
		else
		{
			if(cameraView == null)
				cameraView = new CameraView(collectorUI.getContext());
			cameraView.update();

			if (field.getCount(controller.getCurrentRecord()) == 0 || !field.isMultiple() || goToCapture) {
				// if no photos, not multiple, or just came from gallery then go to capture UI
				return cameraView;
			}
			else {
				// else go to gallery (e.g. if have just taken a photo / if skipping back)
				if (galleryView == null) {
					galleryView = new GalleryView(collectorUI.getContext());
				}
				galleryView.update();
				return galleryView;
			}
		}
	}

	private class CameraView extends ViewSwitcher implements PictureCallback
	{

		static private final String TAG = "CameraView";

		static private final int PREVIEW_SIZE = 1024;

		// UI elements:
		private LinearLayout captureLayout;
		private LinearLayout reviewLayout; // Layout for reviewing single photos
		private ImageView reviewView; // Container for the newly taken image

		// Camera & image data:
		private CameraController cameraController;
		private HandleImage handleImage;
		private byte[] reviewPhotoData;

		@SuppressWarnings("deprecation")
		public CameraView(Context context)
		{
			super(context);

			// Set up cameraController:
			//	Camera controller & camera selection:
			cameraController = new CameraController(field.isUseFrontFacingCamera());
			if(!cameraController.foundCamera())
			{ // no camera found, try the other one:
				cameraController.findCamera(!field.isUseFrontFacingCamera());
				if(!cameraController.foundCamera())
				{ // still no camera, this device does not seem to have one:
					mediaDone(null, false);
					return;
				}
			}
			//	Set flash mode:
			cameraController.setFlashMode(field.getFlashMode());

			// --- Capture UI:
			captureLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_capture, this, false);

			// Create the surface for previewing the camera:
			final SurfaceView surfaceView = (SurfaceView) captureLayout.findViewById(R.id.capture_layout_surface);

			// Set-up surface holder:
			SurfaceHolder holder = surfaceView.getHolder();
			holder.addCallback(cameraController);
			holder.setKeepScreenOn(true);
			// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
			holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);

			// Add the Capture button:
			final LinearLayout captureLayoutButtons = (LinearLayout) captureLayout.findViewById(R.id.capture_layout_buttons);
			captureLayoutButtons.addView(new CaptureButtonView(getContext()));

			// Add the CaptureLayout to the screen
			this.addView(captureLayout);

			// --- Review UI:
			reviewLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_review, this, false);
			reviewView = (ImageView) reviewLayout.findViewById(R.id.review_layout_imageview);
			reviewView.setScaleType(ScaleType.FIT_CENTER);

			// Add the confirm/cancel buttons:
			final LinearLayout reviewLayoutButtons = (LinearLayout) reviewLayout.findViewById(R.id.review_layout_buttons);
			reviewLayoutButtons.addView(new ReviewButtonView(getContext()));

			// Add the ReviewLayout to the screen
			this.addView(reviewLayout);

		}

		public void update()
		{
			// switch back to capture UI if necessary:
			if (getCurrentView() == reviewLayout)
				showNext();
		}

		public void finalise()
		{
			if(cameraController != null)
				cameraController.close();
		}

		@Override
		public void onPictureTaken(byte[] data, Camera camera)
		{
			this.reviewPhotoData = data;
			handleImage = new HandleImage(data, reviewView);
			handleImage.execute();
		}


		/**
		 * AsyncTask to handle the Captured Images
		 * 
		 * @author Michalis Vitos
		 * 
		 */
		public class HandleImage extends AsyncTask<Void, Void, Bitmap>
		{
			private ProgressDialog dialog;
			private byte[] data;
			private ImageView reviewView;

			public HandleImage(byte[] data, ImageView reviewView)
			{
				this.data = data;
				this.reviewView = reviewView;
			}

			@Override
			protected void onPreExecute()
			{
				dialog = new ProgressDialog(CameraView.this.getContext());
				dialog.setCancelable(false);
				dialog.show();
			}

			@Override
			protected Bitmap doInBackground(Void... params)
			{
				Bitmap picture = null;

				try
				{
					// TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488

					// Decode image size, do not create the actual bitmap (picture is null)
					BitmapFactory.Options options = new BitmapFactory.Options();
					options.inJustDecodeBounds = true;
					picture = BitmapFactory.decodeByteArray(data, 0, data.length, options);

					// Find the preview size
					int previewWidth = (ScreenMetrics.GetScreenWidth(getContext()) > 0) ? ScreenMetrics.GetScreenWidth(getContext()) : PREVIEW_SIZE;
					int previewHeight = (ScreenMetrics.GetScreenHeight(getContext()) > 0) ? ScreenMetrics.GetScreenHeight(getContext()) : PREVIEW_SIZE;

					// Decode with inSampleSize and get the correct, scaled image
					options.inJustDecodeBounds = false;
					options.inSampleSize = BitmapUtils.calculateInSampleSize(options, previewWidth, previewHeight);
					picture = BitmapFactory.decodeByteArray(data, 0, data.length, options);
				}
				catch(Exception e)
				{
					Log.e("Handle image", "Image capture failed");
					Debug.e(e);
				}

				return picture;
			}

			@Override
			protected void onPostExecute(Bitmap picture)
			{
				// Display rotated picture
				reviewView.setImageBitmap(picture);
				// Switch to review mode:
				cameraController.stopPreview();
				showNext();
				// Close the dialog
				dialog.cancel();
				handlingClick.release();
			}
		}


		private class CaptureButtonView extends CameraButtonView
		{

			public CaptureButtonView(Context context)
			{
				super(context);

				buttonAction = new Runnable() {
					@Override
					public void run() {
						goToCapture = false; //just taken a photo, so go to gallery instead (unless multiple disabled)
						cameraController.takePicture(CameraView.this);
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
			 * @see uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.CameraView.CameraButtonView#addButtons()
			 */
			@Override
			protected void addButtons()
			{
				// Capture button:
				Item captureButton = null;
				File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
				if(FileHelpers.isReadableFile(captureImgFile))
					captureButton = new FileImageItem(captureImgFile);
				else
					captureButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_photo_svg);
				captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(captureButton);
			}
		}

		/**
		 * @author mstevens
		 */
		private class ReviewButtonView extends CameraButtonView
		{

			public ReviewButtonView(Context context)
			{
				super(context);
				setHorizontalSpacing(collectorUI.getSpacingPx());

				buttonAction = new Runnable() {
					@Override
					public void run() {
						//there are 2 buttons: approve (pos=0) & discard (pos=1)
						if (position == 0) { // photo approved
							try { // Save photo to file:
								File photoFile =
										field.getNewTempFile(controller
												.getCurrentRecord());
								FileOutputStream fos =
										new FileOutputStream(photoFile);
								fos.write(reviewPhotoData);
								fos.close();
								if (field.isMultiple()) {
									mediaAddedButNotDone(photoFile);										
									//TODO "re enter" by using getPlatformView? mediaDone instead?
								} else {
									mediaDone(photoFile, true);
									cameraController.close();
								}

							} catch (Exception e) {
								mediaDone(null, true);
								cameraController.close();
							}
						} else 
						{ // position == 1, photo discarded
							showNext(); // switch back to capture mode
							cameraController.startPreview();
						}
						reviewPhotoData = null;
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

		private LinearLayout pickerLayout;
		private PhotoPickerView photoPicker;
		private LinearLayout deleteLayout;
		private LinearLayout pickerLayoutButtons;
		private ImageView deletePhotoView;
		
		private boolean maxReached = false; // whether or not max no. photos taken

		public GalleryView(Context context) {
			super(context);
			this.context = context;

			// Multiple photos can be taken, so use a PickerView for review after confirmation:
			pickerLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_picker, this, false);

			// Add the confirm/cancel buttons:
			pickerLayoutButtons = (LinearLayout) pickerLayout.findViewById(R.id.picker_layout_buttons);
			pickerLayoutButtons.addView(new PickerButtonView(context));
			final LinearLayout pickerViewContainer = (LinearLayout) pickerLayout.findViewById(R.id.picker_layout_picker_container);
			refreshPickerButtons();
			photoPicker = new PhotoPickerView(context);
			pickerViewContainer.addView(photoPicker);
			photoPicker.loadPhotos();
			// Add the picker:
			this.addView(pickerLayout);

			// Add the layout for photo review/deletion after capture:
			deleteLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_review, this, false);
			deletePhotoView = (ImageView) deleteLayout.findViewById(R.id.review_layout_imageview);
			deletePhotoView.setScaleType(ScaleType.FIT_CENTER);

			// Add the confirm/cancel buttons:
			final LinearLayout deleteLayoutButtons = (LinearLayout) deleteLayout.findViewById(R.id.review_layout_buttons);
			deleteLayoutButtons.addView(new DeleteButtonView(getContext()));

			// Add the view:
			addView(deleteLayout);
		}

		public void update() {
			photoPicker.loadPhotos();	        
		}

		public void refreshPickerButtons() {
			pickerLayoutButtons.removeAllViews();
			pickerLayoutButtons.addView(new PickerButtonView(context));
        }

		private void showPhotoDeleteLayout(File photoFile) {
			// set the ImageView to the provided photo file:
			deletePhotoView.setImageURI(Uri.fromFile(photoFile));
			// Show the view:
			showNext();
		}

		private class PhotoPickerView extends PickerView {

			private static final int NUM_COLUMNS = 3; //TODO make configurable?
			private static final int NUM_ROWS = 3;

			private int photoPosition;

			public PhotoPickerView(Context context) {
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
						// a photo has been clicked, so show it and offer deletion
						showPhotoDeleteLayout(((FileImageItem)getAdapter().getItem(position)).getFile());
					}	
				});
			}

			protected void loadPhotos() {
				PickerAdapter adapter = new PickerAdapter();
				List<File> photoFiles = controller.getMediaAttachments(); //TODO might not be photos!
				for (File f : photoFiles) {
					FileImageItem imgItem = new FileImageItem(f);
					adapter.addItem(imgItem);
				}
				setAdapter(adapter);
				if (photoFiles.size() >= field.getMax()) {
					maxReached = true;
					GalleryView.this.refreshPickerButtons();
				}
				getAdapter().notifyDataSetChanged();
			}

			protected void deleteCurrentPhoto() {
				FileImageItem currentPhotoItem = (FileImageItem)getAdapter().getItem(photoPosition);
				File currentPhotoFile = currentPhotoItem.getFile();
				removeMedia(currentPhotoFile);
				getAdapter().removeItem(currentPhotoItem);
				maxReached = false;  // have now deleted a photo, so cannot have reached max
				GalleryView.this.refreshPickerButtons();
				getAdapter().notifyDataSetChanged();
			}
		}


		/**
		 * @author mstevens
		 */
		private class DeleteButtonView extends CameraButtonView
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
							// "delete" selected, delete photo
							photoPicker.deleteCurrentPhoto();
							if (field.getCount(controller.getCurrentRecord()) == 0) {
								// no photos left, so go back to capture
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

		private class PickerButtonView extends CameraButtonView
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
				Item captureButton = null;
				File captureImgFile = controller.getProject().getImageFile(field.getCaptureButtonImageRelativePath());
				if(FileHelpers.isReadableFile(captureImgFile))
					captureButton = new FileImageItem(captureImgFile);
				else
					captureButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_photo_svg);
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
	private abstract class CameraButtonView extends PickerView
	{
		private int buttonPadding;
		private int buttonBackColor;
		Runnable buttonAction;
		int position;
		/**
		 * @param context
		 */
		public CameraButtonView(Context context)
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
						CameraButtonView.this.position = position;
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
