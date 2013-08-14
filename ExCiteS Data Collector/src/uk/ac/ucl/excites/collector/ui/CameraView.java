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
import uk.ac.ucl.excites.collector.ui.picker.ImageFileItem;
import uk.ac.ucl.excites.collector.ui.picker.ImageResourceItem;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.util.FileHelpers;
import android.app.ProgressDialog;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.Matrix;
import android.hardware.Camera;
import android.hardware.Camera.PictureCallback;
import android.os.AsyncTask;
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
 * To do's: - TODO Fix white "blocks" briefly appearing where the button(s) should be when view is loaded for 2nd time
 * 
 * @author mstevens, Michalis Vitos
 */
public class CameraView extends ViewSwitcher implements FieldView, AdapterView.OnItemClickListener, PictureCallback
{

	static private final String TAG = "CameraView";
	static private final int PREVIEW_SIZE = 1024;

	private Context context;

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
	private HandleImage handleImage;

	public CameraView(Context context)
	{
		super(context);

		this.context = context;

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

		handleImage = new HandleImage(data, reviewView);
		handleImage.execute();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id)
	{
		if(!handlingClick) // only handle one click at the time
		{
			handlingClick = true;
			if(getCurrentView() == captureLayout)
			{ // in Capture mode --> there is (currently) only one button here: the one to take a photo
				cameraController.takePicture(this);
			}
			else
			{ // in Review mode --> there are 2 buttons: approve (pos=0) & discard (pos=1)
				if(position == 0)
				{ // photo approved
					try
					{ // Save photo to file:
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
				else
				// if(position == 1)
				{ // photo discarded
					showNext(); // switch back to capture mode
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
			File captureImgFile = project.getImageFile(photoField.getCaptureButtonImageRelativePath());
			if(FileHelpers.isReadableFile(captureImgFile))
				pickerAdapter.addItem(new ImageFileItem(captureImgFile));
			else
				pickerAdapter.addItem(new ImageResourceItem(R.drawable.take_photo));
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
			File approveImgFile = project.getImageFile(photoField.getApproveButtonImageRelativePath());
			if(FileHelpers.isReadableFile(approveImgFile))
				pickerAdapter.addItem(new ImageFileItem(approveImgFile));
			else
				pickerAdapter.addItem(new ImageResourceItem(R.drawable.accept));
			// Discard button:
			File discardImgFile = project.getImageFile(photoField.getDiscardButtonImageRelativePath());
			if(FileHelpers.isReadableFile(discardImgFile))
				pickerAdapter.addItem(new ImageFileItem(discardImgFile));
			else
				pickerAdapter.addItem(new ImageResourceItem(R.drawable.delete));
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
			setPadding(0, 0, 0, getSpacingInDp(context));

			// Columns
			setNumColumns(getNumberOfColumns());

			// Images/buttons:
			pickerAdapter = new PickerAdapter(super.getContext());
			addButtons();

			// Button dimensions:
			pickerAdapter.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
			pickerAdapter.setItemWidth(buttonSize);
			pickerAdapter.setItemHeight(buttonSize);
			// Button background colour:
			try
			{
				pickerAdapter.setBackgroundColor(Color.parseColor(controller.getCurrentForm().getButtonBackgroundColor()));
			}
			catch(IllegalArgumentException iae)
			{
				Log.w(CameraView.TAG, "Unable to parse colour: " + controller.getCurrentForm().getButtonBackgroundColor());
				pickerAdapter.setBackgroundColor(Color.parseColor(Form.DEFAULT_BUTTON_BACKGROUND_COLOR)); // light gray
			}
			// And finally:
			setAdapter(pickerAdapter);
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
			dialog = new ProgressDialog(context);
			dialog.setCancelable(false);
			dialog.show();
		}

		@Override
		protected Bitmap doInBackground(Void... params)
		{
			Bitmap picture = BitmapFactory.decodeByteArray(data, 0, data.length);
			return scaleAndRotate(picture);
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

		protected Bitmap scaleAndRotate(Bitmap picture)
		{
			// TODO use EXIF data to determine proper rotation? Cf. http://stackoverflow.com/q/12944123/1084488

			// Find the Aspect Ratio
			Float width = Float.valueOf(picture.getWidth());
			Float height = Float.valueOf(picture.getHeight());
			Float ratio = width / height;

			// Scale
			picture = Bitmap.createScaledBitmap(picture, (int) (PREVIEW_SIZE * ratio), PREVIEW_SIZE, false);

			// Rotate
			Matrix bitmapMatrix = new Matrix();
			bitmapMatrix.postRotate(90);

			return Bitmap.createBitmap(picture, 0, 0, picture.getWidth(), picture.getHeight(), bitmapMatrix, false);
		}
	}

}
