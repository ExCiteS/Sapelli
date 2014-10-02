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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.media.CameraController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
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
public class AndroidPhotoUI extends PhotoUI<View, CollectorView>
{
	
	private CameraView photoView;
	
	public AndroidPhotoUI(PhotoField field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	@Override
	protected void cancel()
	{
		if(photoView != null)
		{
			photoView.finalise();
			photoView = null;
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
			if(photoView == null)
				photoView = new CameraView(collectorUI.getContext());
			
			// Init/reset the view:
			photoView.update();
			
			// Return view:
			return photoView;
		}
	}
	
	private class CameraView extends ViewFlipper implements PictureCallback
	{

		static private final String TAG = "CameraView";
		
		static private final int PREVIEW_SIZE = 1024;
		
		// UI elements:
		private LinearLayout captureLayout;
		private LinearLayout reviewLayout; // Layout for reviewing single photos
		private ImageView reviewView; // Container for the newly taken image
		
		// Objects for multi-photo review:
		private FileImageItem imgItem;
		private LinearLayout pickerLayout;
		private PhotoPickerView photoPicker;

		// Camera & image data:
		private CameraController cameraController;
		private HandleImage handleImage;
		private byte[] reviewPhotoData;
		
		private volatile boolean handlingClick;

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
			captureLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_capture, null);

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
			reviewLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_review, null);
			reviewView = (ImageView) reviewLayout.findViewById(R.id.review_layout_imageview);
			reviewView.setScaleType(ScaleType.FIT_CENTER);
			
			// Add the confirm/cancel buttons:
			final LinearLayout reviewLayoutButtons = (LinearLayout) reviewLayout.findViewById(R.id.review_layout_buttons);
			reviewLayoutButtons.addView(new ReviewButtonView(getContext()));
			
			// Add the ReviewLayout to the screen
			this.addView(reviewLayout);
		
			if (field.isMultiple()) {
				// Multiple photos can be taken, so use a PickerView for review after confirmation
				pickerLayout = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.collector_camera_picker, null);
				
				// Add the confirm/cancel buttons:
				final LinearLayout pickerLayoutButtons = (LinearLayout) pickerLayout.findViewById(R.id.picker_layout_buttons);
				pickerLayoutButtons.addView(new PickerButtonView(getContext()));
				final LinearLayout pickerViewContainer = (LinearLayout) pickerLayout.findViewById(R.id.picker_layout_picker_container);
				photoPicker = new PhotoPickerView(context);
				pickerViewContainer.addView(photoPicker);
				
				// Add the picker too (may want to change how this works)
				this.addView(pickerLayout);
			}
		}
		
		public void update()
		{
			this.handlingClick = false;
			// Switch back to capture layout if needed:
			if(getCurrentView() == reviewLayout)
				showPrevious();
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
		
		private class PhotoPickerView extends PickerView {
			
			private static final int NUM_COLUMNS = 3; //TODO make configurable?
			private static final int NUM_ROWS = 3;

			public PhotoPickerView(Context context) {
	            this(context, true);
            }
			
			public PhotoPickerView(Context context, Boolean recycleViews) {
	            super(context, recycleViews);
	        
				// Number of columns:
	            setNumColumns(NUM_COLUMNS);
				// Item size & padding:
				setItemDimensionsPx(
						collectorUI.getFieldUIPartWidthPx(NUM_COLUMNS),
						collectorUI.getFieldUIPartHeightPx(NUM_ROWS));
				
				// Add a "capture more photos" button to the picker by default:
                getAdapter().addItem(new ResourceImageItem(
                				getContext().getResources(), R.drawable.button_photo_svg));
				
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
						if (!handlingClick) {
							handlingClick = true;
							cameraController.takePicture(CameraView.this);	
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
						if (!handlingClick) {
							//there are 2 buttons: approve (pos=0) & discard (pos=1)
							handlingClick = true;
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
			                            // create an ImageItem from the capture so it can be shown in a PickerView
			                            imgItem =
			                                    new FileImageItem(photoFile);
			                            photoPicker.getAdapter().addItem(
			                                    imgItem);
			                            //show multi-preview panel
			                            showNext();
		                            } else {
			                            mediaDone(photoFile, true);
		                            }

	                            } catch (Exception e) {
		                            Log.e(TAG, "Could not save photo.", e);
		                            mediaDone(null, true);
	                            } finally {
		                            cameraController.close();
	                            }
                            } else 
                            { // position == 1, photo discarded
	                            showPrevious(); // switch back to capture mode
	                            cameraController.startPreview();
	                            handlingClick = false;
                            }
                            reviewPhotoData = null;
						}
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
					discardButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_delete_svg);
				discardButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(discardButton);
			}

		}
		
		private class PickerButtonView extends CameraButtonView
		{

			public PickerButtonView(Context context)
			{
				super(context);
				
				buttonAction = new Runnable() {
					@Override
                    public void run() {
						if (!handlingClick) {
							handlingClick = true;
							//current view is pickerLayout --> there is only an "approve" button
							mediaDone(null, true);  
							cameraController.close();
							handlingClick = false;
						}
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
				// Approve button:
				Item approveButton = null;
				File approveImgFile = controller.getProject().getImageFile(field.getApproveButtonImageRelativePath());
				if(FileHelpers.isReadableFile(approveImgFile))
					approveButton = new FileImageItem(approveImgFile);
				else
					approveButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_tick_svg);
				approveButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
				addButton(approveButton);
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
						CameraButtonView.this.position = position;
						// Execute the "press" animation if allowed, then perform the action: 
						if(controller.getCurrentForm().isClickAnimation())
							(new ClickAnimator(buttonAction, view, collectorUI)).execute(); //execute animation and the action afterwards
						else
							buttonAction.run(); //perform task now (animation is disabled)
                    }	
				});

				// Layout:
				setBackgroundColor(Color.TRANSPARENT);
				setGravity(Gravity.CENTER);
				setPadding(0, collectorUI.getSpacingPx(), 0, 0);

				// Columns
				setNumColumns(getNumberOfColumns());

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
				showNext();
				handlingClick = false;

				cameraController.stopPreview();
				// Close the dialog
				dialog.cancel();
			}
		}

	}
	
}
