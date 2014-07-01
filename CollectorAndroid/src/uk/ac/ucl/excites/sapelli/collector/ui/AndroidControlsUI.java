/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.HorizontalArrow;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.widget.AbsListView.LayoutParams;
import android.widget.AdapterView;

/**
 * Android implementation of ControlsUI
 * 
 * @author mstevens
 */
public class AndroidControlsUI extends ControlsUI<View, CollectorView> implements AdapterView.OnItemClickListener
{
	
	// Statics-------------------------------------------------------
	static public final float CONTROL_HEIGHT_DIP = 60.0f;
	static public final float PADDING_DIP = 6.0f;
	static public final int FOREGROUND_COLOR = Color.BLACK;
	static private final int SEMI_TRANSPARENT_WHITE = Color.parseColor("#80FFFFFF");
	
	// Dynamics------------------------------------------------------
	private ControlItem[] controlItems;
	private PickerView view;
	
	public AndroidControlsUI(Controller controller, CollectorView collectorView)
	{
		super(controller, collectorView);
		
		// ControlItem array:
		this.controlItems = new ControlItem[Control.values().length];
	}

	@Override
	protected View getPlatformView()
	{
		if(view == null)
		{
			view = new PickerView(collectorUI.getContext());
			
			// UI set-up:
			view.setBackgroundColor(Color.BLACK);
			view.setHorizontalSpacing(collectorUI.getSpacingPx());
			view.setPadding(0, 0, 0, collectorUI.getSpacingPx()); // Bottom padding (to put spacing between buttons and view underneath)
			
			// ControlItem size:
			view.setItemDimensionsPx(LayoutParams.MATCH_PARENT, getControlHeightPx());
			
			// Listen for clicks:
			view.setOnItemClickListener(this);
		}
			
		return view;
	}
	
	@Override
	public void disable()
	{
		super.disable();
		if(view != null)
			view.setEnabled(false);
	}
	
	public void enable()
	{
		super.enable();
		if(view != null)
			view.setEnabled(true);
	}

	@Override
	protected void updateForm(Form newForm)
	{
		if(view == null)
			return;
		
		/* Disable view recycling if animation enabled, in order to work around an Android bug
		 * 	On recent Android versions (observed on v4.1.2/Xcover2 & v4.4.2/Nexus4; but not on v2.3.6/Xcover1)
		 * 	the press animation leaves behind a trailing control view in some cases. The only workaround we've
		 * 	found so far (other than disabling the press animation on the controls, which we don't want to do)
		 * 	is to always create now views for the control items. */
		view.setRecycleViews(!newForm.isAnimation());
		
		// Background colour:
		int controlBackgroundColor = ColourHelpers.ParseColour(newForm.getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR); //default is light gray
		
		// (Re)instantiate control items (i.e. buttons):
		for(Control control : ControlsUI.Control.values())
			controlItems[control.ordinal()] = new ControlItem(collectorUI.getContext(), control, newForm, controlBackgroundColor);
	}
	
	@Override
	protected void updateControlStates(State[] newControlStates)
	{
		if(view == null)
			return;
		
		// Update shown controlItems:
		PickerAdapter adapter = view.getAdapter();
		adapter.clear();
		for(Control control : ControlsUI.Control.values())
		{
			State state = newControlStates[control.ordinal()]; 
			if(state != State.HIDDEN)
			{
				controlItems[control.ordinal()].setGrayedOut(state == State.SHOWN_DISABLED);
				adapter.addItem(controlItems[control.ordinal()]);
			}
		}
		
		// Are any controls shown?
		if(!adapter.isEmpty())
		{	// Yes...
			view.setVisibility(View.VISIBLE);
			
			// Columns:
			view.setNumColumns(adapter.getCount());

			// Reset adapter:
			view.setAdapter(adapter);
		}
		else
		{	// No...
			view.setVisibility(View.GONE);
		}
	}
	
	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, final long id)
	{
		// Are we allowed to trigger an action?
		if(!enabled || view == null)
			return; // ignore the click if controls are disabled
		
		// Action triggered by click:
		Runnable action = new Runnable()
		{
			public void run()
			{
				handleControlEvent(Control.values()[(int) id], false);
			}
		};

		// Execute the "press" animation if allowed, then perform the action: 
		if(controller.getCurrentForm().isAnimation())
			(new PressAnimator(action, v, collectorUI)).execute(); //execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)
	}
	
	@Override
	public int getCurrentHeightPx()
	{
		return view == null ? 0 : (view.getAdapter().isEmpty() ? 0 : (getControlHeightPx() + collectorUI.getSpacingPx()));
	}
	
	private int getControlHeightPx()
	{
		return ScreenMetrics.ConvertDipToPx(collectorUI.getContext(), CONTROL_HEIGHT_DIP);
	}
	
	/**
	 * ControlItem, representing a control button
	 * Can be grayed out.
	 * 
	 * @author mstevens
	 */
	private class ControlItem extends LayeredItem
	{
	
		// Overlay to gray-out disabled (but shown) buttons
		private Item grayOutOverlay;
	
		public ControlItem(Context context, Control control, Form form, int backgroundColor)
		{
			// Pass control ordinal as id:
			super(control.ordinal());
			
			// Background & padding:
			this.setBackgroundColor(backgroundColor);
			this.setPaddingPx(0);
			
			// the actual button:
			String imgRelativePath = null;
			Drawable drawable = null;
			switch(control)
			{
				case BACK:
					imgRelativePath = form.getBackButtonImageRelativePath();
					drawable = new HorizontalArrow(FOREGROUND_COLOR, true);
					break;
				case CANCEL:
					imgRelativePath = form.getCancelButtonImageRelativePath();
					drawable = new SaltireCross(FOREGROUND_COLOR);
					break;
				case FORWARD:
					imgRelativePath = form.getForwardButtonImageRelativePath();
					drawable = new HorizontalArrow(FOREGROUND_COLOR, false);
					break;
			}				
			File imgFile = controller.getProject().getImageFile(imgRelativePath);
			Item button = null;
			if(FileHelpers.isReadableFile(imgFile))
				button = new FileImageItem(imgFile);
			else
				button = new DrawableItem(drawable);
			/* Unused -- replaced by Drawable buttons (arrow & cross)
			// Resource image (e.g. R.drawable.button_back_svg, .button_back, .button_delete_svg, .button_delete, .button_forward_svg, .button_forward)
			button = new ResourceImageItem(getContext().getResources(), R.drawable.button_back_svg); */
			button.setPaddingPx(ScreenMetrics.ConvertDipToPx(context, PADDING_DIP));
			
			// the overlay
			grayOutOverlay = new EmptyItem();
			grayOutOverlay.setPaddingPx(0);
			setGrayedOut(false);
			
			// add the layers:
			this.addLayer(button, true);
			this.addLayer(grayOutOverlay, false);
		}
		
		public void setGrayedOut(boolean grayedOut)
		{
			grayOutOverlay.setBackgroundColor(grayedOut ? SEMI_TRANSPARENT_WHITE : Color.TRANSPARENT);
		}
		
	}

}
