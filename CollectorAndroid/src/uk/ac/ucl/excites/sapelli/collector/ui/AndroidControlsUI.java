/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui;

import java.io.File;
import java.util.Arrays;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
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
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.widget.AdapterView;

/**
 * Android implementation of ControlsUI
 * 
 * @author mstevens
 */
public class AndroidControlsUI extends ControlsUI<View, CollectorView>
{
	
	// Statics-------------------------------------------------------
	static public final float CONTROL_HEIGHT_DIP = 60.0f;
	static public final float PADDING_DIP = 6.0f;
	static public final int FOREGROUND_COLOR = Color.BLACK;
	static private final int SEMI_TRANSPARENT_WHITE = Color.parseColor("#80FFFFFF");
	
	// Dynamics------------------------------------------------------
	private ControlsView view;

	public AndroidControlsUI(Controller controller, CollectorView collectorView)
	{
		super(controller, collectorView);
	}

	@Override
	protected View getPlatformView()
	{
		if(view == null)
			view = new ControlsView(collectorUI.getContext());
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
	public void update(FieldUI<?, View, CollectorView> currentFieldUI)
	{
		if(view != null)
			view.update(controller.getCurrentForm(), currentFieldUI);
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
	 * @author mstevens, Julia, Michalis Vitos
	 * 
	 */
	private class ControlsView extends PickerView implements AdapterView.OnItemClickListener
	{
	
		private State[] controlStates;
		private ControlItem[] controlItems;
		private Form currentForm;
		
		/**
		 * @param context
		 */	
		public ControlsView(Context context)
		{
			super(context);
			
			// UI set-up:
			setBackgroundColor(Color.BLACK);
			setHorizontalSpacing(collectorUI.getSpacingPx());
			setPadding(0, 0, 0, collectorUI.getSpacingPx()); // Bottom padding (to put spacing between buttons and view underneath)
			
			// ControlItem array:
			this.controlItems = new ControlItem[Control.values().length];
			
			// ControlItem size:
			setItemDimensionsPx(LayoutParams.MATCH_PARENT, getControlHeightPx());
			
			// Listen for clicks:
			setOnItemClickListener(this);
		}

		public void update(Form form, FieldUI<?, View, CollectorView> fieldUI)
		{
			// Form change?
			if(currentForm != form)
			{
				currentForm = form;
				
				// Background colour:
				int controlBackgroundColor = ColourHelpers.ParseColour(currentForm.getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR); //default is light gray
				
				// (Re)instantiate control items (i.e. buttons):
				for(Control control : ControlsUI.Control.values())
				{
					controlItems[control.ordinal()] = new ControlItem(control, controlBackgroundColor);
				}
			}
			
			// What do we need to show?
			State[] newControlStates = new State[Control.values().length];
			for(Control control : ControlsUI.Control.values())
				newControlStates[control.ordinal()] = fieldUI.getControlState(control); // takes into account the current FormMode
			
			// Is this different from the currently shown controls?
			if(!Arrays.equals(controlStates, newControlStates))
			{
				controlStates = newControlStates;
				
				// Update shown controlItems:
				PickerAdapter adapter = getAdapter();
				adapter.clear();
				for(Control control : ControlsUI.Control.values())
				{
					State state = controlStates[control.ordinal()]; 
					if(state != State.HIDDEN)
					{
						adapter.addItem(controlItems[control.ordinal()]);
						controlItems[control.ordinal()].setGrayedOut(state == State.SHOWN_DISABLED);
					}	
				}
				
				// Are any controls shown?
				if(!adapter.isEmpty())
				{	// Yes...
					setVisibility(View.VISIBLE);
					
					// Columns:
					setNumColumns(adapter.getItems().size());

					// Set adapter:
					setAdapter(adapter); // only needed on Android 2.3.x?
				}
				else
				{	// No...
					setVisibility(View.GONE);
				}
			}
		}
		
		@Override
		public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
		{
			// Are we allowed to trigger an action?
			if(!enabled || position < 0 || position >= getAdapter().getItems().size())
				return; // ignore the click if buttons are disabled or invalid button was somehow pressed
			
			// Action triggered by click:
			Runnable action = new Runnable()
			{
				public void run()
				{
					// Compute position offset (accounting for hidden controls):
					int positionOffset = 0;
					for(int p = 0; p <= position; p++)
						if(controlStates[p] == State.HIDDEN)
							positionOffset++;
					if(controlStates[position + positionOffset] == State.SHOWN_ENABLED)
						onControlClick(Control.values()[position + positionOffset]);
				}
			};

			// Execute the "press" animation if allowed, then perform the action: 
			if(controller.getCurrentForm().isAnimation())
				(new PressAnimator(action, v, collectorUI)).execute(); //execute animation and the action afterwards
			else
				action.run(); //perform task now (animation is disabled)
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
		
			public ControlItem(Control control, int backgroundColor)
			{
				// Background & padding:
				this.setBackgroundColor(backgroundColor);
				this.setPaddingPx(0);
				
				// the actual button:
				String imgRelativePath = null;
				Drawable drawable = null;
				switch(control)
				{
					case BACK:
						imgRelativePath = currentForm.getBackButtonImageRelativePath();
						drawable = new HorizontalArrow(FOREGROUND_COLOR, true);
						break;
					case CANCEL:
						imgRelativePath = currentForm.getCancelButtonImageRelativePath();
						drawable = new SaltireCross(FOREGROUND_COLOR);
						break;
					case FORWARD:
						imgRelativePath = currentForm.getForwardButtonImageRelativePath();
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
				button.setPaddingPx(ScreenMetrics.ConvertDipToPx(getContext(), PADDING_DIP));
				
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

}
