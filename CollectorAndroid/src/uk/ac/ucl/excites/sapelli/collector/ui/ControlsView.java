
package uk.ac.ucl.excites.sapelli.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.HorizontalArrow;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
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
	private Controller controller;
	private Form currentForm;
	private ControlsState currentState;
	
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
		
		setItemDimensionsPx(LayoutParams.MATCH_PARENT, getButtonHeightPx());
		setOnItemClickListener(this);
				
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

	public void update(Controller controller)
	{
		this.controller = controller;
		
		// (Re)create button items if needed...
		if(currentForm == null || currentForm != controller.getCurrentForm())
		{
			currentForm = controller.getCurrentForm();
			Project project = controller.getProject();
			
			// Background colour:
			int backColor = ColourHelpers.ParseColour(currentForm.getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR); //default is light gray		
			
			// Back button
			backButton = createButton(project.getImageFile(currentForm.getBackButtonImageRelativePath()), new HorizontalArrow(FOREGROUND_COLOR, true), backColor);
			
			// Cancel button
			cancelButton = createButton(project.getImageFile(currentForm.getCancelButtonImageRelativePath()), new SaltireCross(FOREGROUND_COLOR), backColor);
			
			// Forward button
			forwardButton = createButton(project.getImageFile(currentForm.getForwardButtonImageRelativePath()), new HorizontalArrow(FOREGROUND_COLOR, false), backColor);
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
				
				PickerAdapter pickerAdapter = getAdapter();
				if(pickerAdapter == null)
					pickerAdapter = new PickerAdapter(getContext());
				else
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
	
	private Item createButton(File imgFile, Drawable drawable, int backColor)
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
					// TODO when having switched to enum for button types we can make this code more compact (just having the addLogLine line once using buttontype.toString() instead of the hard coded Strings)
					case BUTTON_TYPE_BACK :
						controller.addLogLine("BACK_BUTTON", controller.getCurrentField().getID());
						controller.goBack(true);
						break;
					case BUTTON_TYPE_CANCEL : 
						controller.addLogLine("CANCEL_BUTTON", controller.getCurrentField().getID());
						controller.cancelAndRestartForm();
						break;
					case BUTTON_TYPE_FORWARD :
						controller.addLogLine("FORWARD_BUTTON", controller.getCurrentField().getID());
						controller.goForward(true);
						break;
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
