
package uk.ac.ucl.excites.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.ButtonsState;
import uk.ac.ucl.excites.collector.ui.picker.ImageFileItem;
import uk.ac.ucl.excites.collector.ui.picker.ImageResourceItem;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.util.FileHelpers;
import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
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
				setPadding(0, 0, 0, getSpacingInDp(getContext()));
				
				// Adapter & images
				pickerAdapter = new PickerAdapter(super.getContext());
				int p = 0;
				//	Add buttons:
				if(currentState.isBackShown())
				{
					File bckImgFile = project.getImageFile(form.getBackButtonImageRelativePath());
					if(FileHelpers.isReadableFile(bckImgFile))
						pickerAdapter.addItem(new ImageFileItem(bckImgFile));
					else
						pickerAdapter.addItem(new ImageResourceItem(R.drawable.button_back));
					positionToButton[p++] = BUTTON_TYPE_BACK;
				}
				if(currentState.isCancelShown())
				{
					File cncImgFile = project.getImageFile(form.getCancelButtonImageRelativePath());
					if(FileHelpers.isReadableFile(cncImgFile))
						pickerAdapter.addItem(new ImageFileItem(cncImgFile));
					else
						pickerAdapter.addItem(new ImageResourceItem(R.drawable.button_delete));
					positionToButton[p++] = BUTTON_TYPE_CANCEL;
				}
				if(currentState.isForwardShown())
				{
					File fwdImgFile = project.getImageFile(form.getForwardButtonImageRelativePath());
					if(FileHelpers.isReadableFile(fwdImgFile))
						pickerAdapter.addItem(new ImageFileItem(fwdImgFile));
					else
						pickerAdapter.addItem(new ImageResourceItem(R.drawable.button_forward));
					positionToButton[p++] = BUTTON_TYPE_FORWARD;
				}
				//  Button dimensions:
				pickerAdapter.setScaleType(ImageView.ScaleType.CENTER);
				pickerAdapter.setItemWidth(LayoutParams.MATCH_PARENT);
				pickerAdapter.setItemHeight((int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTON_HEIGHT, getResources().getDisplayMetrics()));
				//  Button background colour:
				try
				{
					pickerAdapter.setBackgroundColor(Color.parseColor(form.getButtonBackgroundColor()));
				}
				catch(IllegalArgumentException iae)
				{
					Log.w(TAG, "Unable to parse colour: " + form.getButtonBackgroundColor());
					pickerAdapter.setBackgroundColor(Color.parseColor(Form.DEFAULT_BUTTON_BACKGROUND_COLOR)); //light gray
				}
			}
			else
			{	//No...
				pickerAdapter = null;
				setPadding(0, 0, 0, 0); //collapse view
			}
			//And finally:
			setAdapter(pickerAdapter); //(no problem if pickerAdapter is null)
		}
	}
	
	@Override
	public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
	{
		// Check if the UI is waiting for animation, if it does do nothing
		if(!CollectorActivity.isWaitingForUIAnimation())
		{
			// Run the animation
			Runnable task = new Runnable()
			{
				public void run()
				{
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
					// else: ignore the click
				}
			};

			Animator animator = new Animator(task, v);
			animator.execute();
		}
		// else: ignore the click
	}

}
