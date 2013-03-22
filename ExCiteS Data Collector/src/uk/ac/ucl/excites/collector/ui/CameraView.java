package uk.ac.ucl.excites.collector.ui;

import java.io.File;
import java.io.FileOutputStream;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.media.CameraController;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.PhotoField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.Matrix;
import android.hardware.Camera;
import android.hardware.Camera.PictureCallback;
import android.util.Log;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.RelativeLayout;
import android.widget.ViewSwitcher;

/**
 * Built-in camera view
 *
  * To do's:
 * 	- TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time
 * 
 * @author mstevens
 */
public class CameraView extends ViewSwitcher implements FieldView, AdapterView.OnItemClickListener, PictureCallback
{

	static private final String TAG = "CameraView";

	private ProjectController controller;
	private Project project;
	private PhotoField photoField;

	private CameraController cameraController;

	private RelativeLayout captureLayout;
	private SurfaceView captureView;
	private RelativeLayout reviewLayout;
	private ImageView reviewView;
	private RelativeLayout.LayoutParams buttonParams;

	private volatile boolean handlingClick;
	
	private byte[] reviewPhotoData;

	public CameraView(Context context)
	{
		super(context);

		// Capture UI:
		captureLayout = new RelativeLayout(context);
		captureView = new SurfaceView(context);
		captureLayout.addView(captureView, LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		// buttons are add in initialise
		this.addView(captureLayout);

		// Review UI:
		reviewLayout = new RelativeLayout(context);
		reviewView = new ImageView(context);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		reviewLayout.addView(reviewView, LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		// buttons are add in initialise
		this.addView(reviewLayout);

		// Layout parameters for the buttons:
		buttonParams = new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT); // You might want to tweak these to WRAP_CONTENT
		buttonParams.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
	}

	@Override
	@SuppressWarnings("deprecation")
	public void initialise(ProjectController controller, Field field)
	{
		this.controller = controller;
		this.project = controller.getProject();
		this.photoField = (PhotoField) field;
		this.handlingClick = false;
		
		// Camera controller & camera selection:
		cameraController = new CameraController(photoField.isUseFrontFacingCamera());
		if(!cameraController.foundCamera())
		{ // no camera found, try the other one:
			cameraController.findCamera(!photoField.isUseFrontFacingCamera());
			if(!cameraController.foundCamera())
			{ // still no camera, this device does not seem to have one:
				controller.mediaDone(null);
				return;
			}
		}
		// Set flash mode:
		cameraController.setFlashMode(photoField.getFlashMode());
		
		// Set-up surface holder:
		SurfaceHolder holder = captureView.getHolder();
		holder.addCallback(cameraController);
		holder.setKeepScreenOn(true);
		// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		
		// Buttons
		captureLayout.addView(new CaptureButtonView(getContext()), buttonParams);
		reviewLayout.addView(new ReviewButtonView(getContext()), buttonParams);
	}

	@Override
	public void onPictureTaken(byte[] data, Camera camera)
	{
		this.reviewPhotoData = data;
		Bitmap picture = BitmapFactory.decodeByteArray(data, 0, data.length);
		Matrix bitmapMatrix = new Matrix();
		bitmapMatrix.postRotate(90);
		// Display rotated picture:
		reviewView.setImageBitmap(Bitmap.createBitmap(picture, 0, 0, picture.getWidth(), picture.getHeight(), bitmapMatrix, false));
		//TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488
		
		//Switch to review mode:
		showNext();
		handlingClick = false;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id)
	{
		if(!handlingClick) //only handle one click at the time
		{
			handlingClick = true;
			if(getCurrentView() == captureLayout)
			{ 	// in Capture mode --> there is (currently) only one button here: the one to take a photo
				cameraController.takePicture(this);
				cameraController.stopPreview();
			}
			else
			{	// in Review mode --> there are 2 buttons: approve (pos=0) & discard (pos=1)
				if(position == 0)
				{ 	// photo approved
					try
					{	// Save photo to file:
						File photoFile = photoField.getNewTempFile(controller.getCurrentRecord());
						FileOutputStream fos = new FileOutputStream(photoFile);
						fos.write(reviewPhotoData);
						fos.close();
						controller.mediaDone(photoFile);
					}
					catch(Exception e)
					{
						Log.e(TAG, "Could not save photo.", e);
						controller.mediaDone(null);
					}
					finally
					{
						cameraController.close();
					}
				}
				else //if(position == 1)
				{	// photo discarded
					showNext(); //switch back to capture mode
					cameraController.startPreview();
					handlingClick = false;
				}
				reviewPhotoData = null;
			}
		}
	}

	@Override
	public View getView()
	{
		return this;
	}

	@Override
	public void cancel()
	{
		cameraController.close();
	}
	
	/**
	 * @author mstevens
	 */
	private class CaptureButtonView extends CameraButtonView
	{

		public CaptureButtonView(Context context)
		{
			super(context);
		}

		@Override
		protected int getNumberOfColumns()
		{
			return 1;
		}

		@Override
		protected void addButtons()
		{
			// Note: for now there is only the capture button, we might add other features (flash, zooming, etc.) later

			// Capture button:
			String captureImg = photoField.getCaptureButtonImageLogicalPath();
			if(captureImg != null && !captureImg.isEmpty())
				imageAdapter.addImage(new FileImage(project, captureImg));
			else
				imageAdapter.addImage(new ResourceImage(R.drawable.take_photo));
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
			String approveImg = photoField.getApproveButtonImageLogicalPath();
			if(approveImg != null && !approveImg.isEmpty())
				imageAdapter.addImage(new FileImage(project, approveImg));
			else
				imageAdapter.addImage(new ResourceImage(R.drawable.accept));
			// Discard button:
			String discardImg = photoField.getDiscardButtonImageLogicalPath();
			if(discardImg != null && !discardImg.isEmpty())
				imageAdapter.addImage(new FileImage(project, discardImg));
			else
				imageAdapter.addImage(new ResourceImage(R.drawable.delete));
		}

	}

	/**
	 * @author mstevens
	 */
	private abstract class CameraButtonView extends PickerView
	{

		static public final float BUTTON_HEIGHT = 64;

		private int buttonSize = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTON_HEIGHT, getResources().getDisplayMetrics());

		/**
		 * @param context
		 */
		public CameraButtonView(Context context)
		{
			super(context);
			setOnItemClickListener(CameraView.this);

			// Layout:
			setBackgroundColor(Color.TRANSPARENT);
			setGravity(Gravity.CENTER);
			setPadding(0, 0, 0, SPACING);

			// Columns
			setNumColumns(getNumberOfColumns());

			// Images/buttons:
			imageAdapter = new ImageAdapter(super.getContext());
			addButtons();

			// Button dimensions:
			imageAdapter.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
			imageAdapter.setImageWidth(buttonSize);
			imageAdapter.setImageHeight(buttonSize);
			// Button background colour:
			try
			{
				imageAdapter.setBackgroundColor(Color.parseColor(controller.getCurrentForm().getButtonBackgroundColor()));
			}
			catch(IllegalArgumentException iae)
			{
				Log.w(CameraView.TAG, "Unable to parse colour: " + controller.getCurrentForm().getButtonBackgroundColor());
				imageAdapter.setBackgroundColor(Color.parseColor(Form.DEFAULT_BUTTON_BACKGROUND_COLOR)); // light gray
			}
			// And finally:
			setAdapter(imageAdapter);
		}

		protected abstract int getNumberOfColumns();

		protected abstract void addButtons();

	}

}
