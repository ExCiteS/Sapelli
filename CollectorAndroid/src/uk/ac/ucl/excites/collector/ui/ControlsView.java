
package uk.ac.ucl.excites.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.collector.*;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.ControlsState;
import uk.ac.ucl.excites.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.collector.ui.drawables.HorizontalArrow;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.collector.ui.picker.items.*;
import uk.ac.ucl.excites.collector.util.ColourHelpers;
import uk.ac.ucl.excites.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.util.io.FileHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, mstevens, Michalis Vitos
 * 
 */
@SuppressLint("ViewConstructor")
public class ControlsView extends PickerView implements AdapterView.OnItemClickListener
{
	
	// Statics-------------------------------------------------------
	static private final String TAG = "ControlsView";
	
	static public final float BUTTON_HEIGHT_DIP = 60.0f;
	
	static public final float PADDING_DIP = 6.0f;
	
	static public final int BUTTON_TYPE_BACK = -1;
	static public final int BUTTON_TYPE_CANCEL = 0;
	static public final int BUTTON_TYPE_FORWARD = 1;
	
	static public final int FOREGROUND_COLOR = Color.BLACK;
	
	// Dynamics------------------------------------------------------
	private CollectorView collectorView;
	
	private boolean enabled;
	private ProjectController controller;
	private Form currentForm;
	private ControlsState currentState;
	
	private int backColor;
	
	private Item backButton;
	private Item cancelButton;
	private Item forwardButton;
	
	private int[] positionToButton;
	
	/**
	 * @param context
	 */	
	public ControlsView(Context context, CollectorView collectorView)
	{
		super(context);
		this.collectorView = collectorView;
		
		enabled = true;
		setOnItemClickListener(this);
		
		// Adapter (setAdapter() is called from update())
		pickerAdapter = new PickerAdapter(super.getContext());
		
		// UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(collectorView.getSpacingPx());
	}
	
	public void disable()
	{
		enabled = false;
	}
	
	public void enable()
	{
		enabled = true;
	}

	public void update(ProjectController controller)
	{
		this.controller = controller;
		
		// Create button items if needed...
		if(currentForm == null || currentForm != controller.getCurrentForm())
		{
			currentForm = controller.getCurrentForm();
			Project project = controller.getProject();
			
			// Background colour:
			backColor = ColourHelpers.ParseColour(currentForm.getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR); //default is light gray		
			
			// Back button 
			if(currentForm.isShowBack())
				backButton = createButton(project.getImageFile(currentForm.getBackButtonImageRelativePath()), new HorizontalArrow(FOREGROUND_COLOR, true));
			else
				backButton = null;
			
			// Cancel button 
			if(currentForm.isShowCancel())
				cancelButton = createButton(project.getImageFile(currentForm.getCancelButtonImageRelativePath()), new SaltireCross(FOREGROUND_COLOR));
			else
				cancelButton = null;
			
			// Forward button 
			if(currentForm.isShowForward())
				forwardButton = createButton(project.getImageFile(currentForm.getForwardButtonImageRelativePath()), new HorizontalArrow(FOREGROUND_COLOR, false));
			else
				forwardButton = null;
		}
		
		// Update state if needed...
		ControlsState newState = controller.getControlsState();
		if(newState == null)
		{
			Log.w(TAG, "Received invalid (null) ButtonState.");
			return;
		}
		if(currentState == null || !currentState.equals(newState))
		{
			currentState = newState;
			
			if(currentState.isAnyButtonShown()) //are there buttons to show?
			{	//Yes...
				// Update position mapping:
				positionToButton = new int[currentState.getNumberOfButtonsShown()];
				
				// Columns
				setNumColumns(positionToButton.length);
				
				pickerAdapter.clear();
				int p = 0;
				//	Add buttons:
				if(currentState.isBackShown())
				{
					pickerAdapter.addItem(backButton);
					positionToButton[p++] = BUTTON_TYPE_BACK;
				}
				if(currentState.isCancelShown())
				{
					pickerAdapter.addItem(cancelButton);
					positionToButton[p++] = BUTTON_TYPE_CANCEL;
				}
				if(currentState.isForwardShown())
				{
					pickerAdapter.addItem(forwardButton);
					positionToButton[p++] = BUTTON_TYPE_FORWARD;
				}
				
				// Set adapter:
				setAdapter(pickerAdapter);
				
				// Bottom padding (to put spacing between buttons and view underneath):
				setPadding(0, 0, 0, ScreenMetrics.ConvertDipToPx(getContext(), CollectorView.SPACING_DIP));
			}
			else
			{	//No...
				setAdapter(null);
				setPadding(0, 0, 0, 0); //collapse view
			}
		}
	}
	
	private Item createButton(File imgFile, Drawable drawable)
	{
		Item button = null;

		// Button image:
		if(FileHelpers.isReadableFile(imgFile))
			button = new FileImageItem(imgFile);
		else
			button = new DrawableItem(drawable);
		
		/* Unused -- replaced by Drawable buttons (arrow & cross)
		// Resource image (e.g. R.drawable.button_back_svg, .button_back, .button_delete_svg, .button_delete, .button_forward_svg, .button_forward)
		button = new ResourceImageItem(getContext().getResources(), R.drawable.button_back_svg);
		*/
		
		// Button size, padding & color:
		button.setWidthPx(LayoutParams.MATCH_PARENT);
		button.setHeightPx(getButtonHeightPx());
		button.setPaddingPx(ScreenMetrics.ConvertDipToPx(getContext(), PADDING_DIP));
		button.setBackgroundColor(backColor);
		
		return button;
	}
	
	public int getButtonHeightPx()
	{
		return ScreenMetrics.ConvertDipToPx(getContext(), BUTTON_HEIGHT_DIP);
	}
	
	@Override
	public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
	{
		// Are we allowed to trigger an action?
		if(!enabled || position < 0 || position >= positionToButton.length)
			return; // ignore the click if buttons are disabled or invalid button was somehow pressed
		
		// Action triggered by click:
		Runnable action = new Runnable()
		{
			public void run()
			{
				switch(positionToButton[position])
				{
					case BUTTON_TYPE_BACK		: controller.goBack(); break;
					case BUTTON_TYPE_CANCEL		: controller.cancelAndRestartForm(); break;
					case BUTTON_TYPE_FORWARD	: controller.goForward(true); break;
					default : return;
				}
			}
		};

		// Execute the "press" animation if allowed, then perform the action: 
		if(controller.getCurrentForm().isAnimation())
			(new PressAnimator(action, v, collectorView)).execute(); //execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)
	}

}
