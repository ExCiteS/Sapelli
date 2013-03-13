package uk.ac.ucl.excites.collector.ui;

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
import android.graphics.Color;
import android.util.Log;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.AdapterView;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

public class CameraView extends FrameLayout implements FieldView, AdapterView.OnItemClickListener
{

	static private final String TAG = "CameraView";

	private ProjectController controller;
	private Project project;
	private PhotoField photoField;
	
	private CameraController cameraController;	
	private boolean capturingMode = true; // if false we are in review mode, we always start in capture mode

	private SurfaceView previewSurface;
	private CameraButtonView buttons;
	
	public CameraView(Context context)
	{
		super(context);

		previewSurface = new SurfaceView(context);
		addView(previewSurface, LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);

		RelativeLayout buttonLayout = new RelativeLayout(context);
		RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, RelativeLayout.LayoutParams.WRAP_CONTENT);
		int margin = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 10, getResources().getDisplayMetrics());
		params.setMargins(margin, 0, margin, margin);
	    buttonLayout.setLayoutParams(params);
		buttonLayout.setGravity(Gravity.BOTTOM);
		
		buttons = new CameraButtonView(context);
		buttons.setGravity(Gravity.BOTTOM);
		buttonLayout.addView(buttons, LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
		addView(buttonLayout);
	}
	
	@Override
	@SuppressWarnings("deprecation")
	public void initialise(ProjectController controller, Field field)
	{
		this.controller = controller;
		this.project = controller.getProject();
		this.photoField = (PhotoField) field;
		
		//Camera controller:
		cameraController = new CameraController();
		if(!cameraController.foundCamera())
		{
			// try back cameraController

			// skip...
		}
		
		SurfaceHolder holder = previewSurface.getHolder();
		holder.addCallback(cameraController);
		holder.setKeepScreenOn(true);
		// !!! Deprecated but cameraController preview crashes without it (at least on the XCover/Gingerbread):
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		
		//Show buttons:
		buttons.update();
	}
	
	@Override
	public void cancel()
	{
		cameraController.close();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id)
	{
		if(capturingMode)
		{	//take photo
			
			cameraController.takePicture();
			cameraController.stopPreview();
			
			//TODO show photo
			
		}
		else
		{
			if(position == 0)
			{	//photo approved
			
				//TODO save photo
	
				cameraController.close();
				controller.photoDone(true);
				return;
			}
			else
			{	//photo discarded
				
				//TODO throw away file
				
				cameraController.startPreview();
			}
		}
		capturingMode = !capturingMode;
		buttons.update();
	}

	/**
	 * @author mstevens
	 * 
	 */
	public class CameraButtonView extends PickerView
	{

		static public final float BUTTON_HEIGHT = 48;

		/**
		 * @param context
		 */
		public CameraButtonView(Context context)
		{
			super(context);

			imageAdapter = new ImageAdapter(super.getContext());

			// Padding
			setPadding(0, SPACING, 0, 0);

			setOnItemClickListener(CameraView.this);
		}
		
		public void update()
		{
			imageAdapter.clear();
			
			if(capturingMode)
			{ // Capture mode

				// Note: for now there is only the capture button, may later we can add flash, focus, etc.?

				// Columns
				setNumColumns(1);

				// Capture button:
				String captureImg = photoField.getCaptureButtonImageLogicalPath();
				if(captureImg != null && !captureImg.isEmpty())
					imageAdapter.addImage(new FileImage(project, captureImg));
				else
					imageAdapter.addImage(new ResourceImage(R.drawable.take_photo));
			}
			else
			{ // Review mode

				// Columns
				setNumColumns(2);

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

			// Button dimensions:
			imageAdapter.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
			imageAdapter.setImageWidth(LayoutParams.WRAP_CONTENT);
			imageAdapter.setImageHeight((int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTON_HEIGHT, getResources().getDisplayMetrics()));
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
			setAdapter(imageAdapter); // (no problem if imageAdapter is null)
		}

	}

	@Override
	public View getView()
	{
		return this;
	}

}
