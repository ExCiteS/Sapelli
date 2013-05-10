
package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.ButtonsState;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
import uk.ac.ucl.excites.util.Debug;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ImageView;

/**
 * @author Julia, mstevens, Michalis Vitos
 * 
 */
public class ButtonView extends PickerView implements AdapterView.OnItemClickListener
{
	
	static private final String TAG = "ButtonView";
	
	static public final float BUTTON_HEIGHT = 65;

	static public final int BUTTON_TYPE_BACK = -1;
	static public final int BUTTON_TYPE_CANCEL = 0;
	static public final int BUTTON_TYPE_FORWARD = 1;
	
	private boolean buttonsEnabled;
	private ProjectController controller;
	private ButtonsState currentState;
	private int[] positionToButton;
	
	/**
	 * @param context
	 */	
	public ButtonView(Context context)
	{
		super(context);
		buttonsEnabled = true;
		setOnItemClickListener(this);
	}
	
	public void disable()
	{
		buttonsEnabled = false;
	}
	
	public void enable()
	{
		buttonsEnabled = true;
	}

	public void update(ProjectController controller)
	{
		this.controller = controller;
		ButtonsState newState = controller.getButtonsState();
		if(newState == null)
		{
			Log.w(TAG, "Received invalid (null) ButtonState.");
			return;
		}
		
		//Should we update at all?
		if(currentState == null || !currentState.equals(newState))
		{	//Yes...
			currentState = newState;
			positionToButton = new int[currentState.getNumberOfButtonsShown()];
			
			if(positionToButton.length >  0) //are there buttons to show?
			{	//Yes...
				// Local variables:
				Project project = controller.getProject();
				Form form = controller.getCurrentForm();
				
				// Columns
				setNumColumns(positionToButton.length);
				
				// Padding
				setPadding(0, 0, 0, SPACING);
				
				// Adapter & images
				imageAdapter = new ImageAdapter(super.getContext());
				int p = 0;
				//	Add buttons:
				if(currentState.isBackShown())
				{
					String bckImg = form.getBackButtonImageLogicalPath(); 
					if(bckImg != null && !bckImg.isEmpty())
						imageAdapter.addImage(new FileImage(project, bckImg));
					else
						imageAdapter.addImage(new ResourceImage(R.drawable.button_back));
					positionToButton[p++] = BUTTON_TYPE_BACK;
				}
				if(currentState.isCancelShown())
				{
					String cncImg = form.getCancelButtonImageLogicalPath(); 
					if(cncImg != null && !cncImg.isEmpty())
						imageAdapter.addImage(new FileImage(project, cncImg));
					else
						imageAdapter.addImage(new ResourceImage(R.drawable.button_delete));
					positionToButton[p++] = BUTTON_TYPE_CANCEL;
				}
				if(currentState.isForwardShown())
				{
					String fwdImg = form.getForwardButtonImageLogicalPath(); 
					if(fwdImg != null && !fwdImg.isEmpty())
						imageAdapter.addImage(new FileImage(project, fwdImg));
					else
						imageAdapter.addImage(new ResourceImage(R.drawable.button_forward));
					positionToButton[p++] = BUTTON_TYPE_FORWARD;
				}
				//  Button dimensions:
				imageAdapter.setScaleType(ImageView.ScaleType.CENTER);
				imageAdapter.setImageWidth(LayoutParams.MATCH_PARENT);
				imageAdapter.setImageHeight((int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTON_HEIGHT, getResources().getDisplayMetrics()));
				//  Button background colour:
				try
				{
					imageAdapter.setBackgroundColor(Color.parseColor(form.getButtonBackgroundColor()));
				}
				catch(IllegalArgumentException iae)
				{
					Log.w(TAG, "Unable to parse colour: " + form.getButtonBackgroundColor());
					imageAdapter.setBackgroundColor(Color.parseColor(Form.DEFAULT_BUTTON_BACKGROUND_COLOR)); //light gray
				}
			}
			else
			{	//No...
				imageAdapter = null;
				setPadding(0, 0, 0, 0); //collapse view
			}
			//And finally:
			setAdapter(imageAdapter); //(no problem if imageAdapter is null)
		}
	}
	
	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id)
	{
		v.startAnimation(AnimationUtils.loadAnimation(getContext(), R.layout.image_click_animation));

		if(buttonsEnabled)
		{
			if(position >= positionToButton.length)
				return;
			//else:
			switch(positionToButton[position])
			{
				case BUTTON_TYPE_BACK		: controller.goBack(); break;
				case BUTTON_TYPE_CANCEL		: controller.cancelAndRestartForm(); break;
				case BUTTON_TYPE_FORWARD	: controller.goForward(true); break;
				default : return;
			}
		}
		//else: ignore the click
	}

}
