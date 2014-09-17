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

package uk.ac.ucl.excites.sapelli.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ViewAnimator;
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
public class AndroidControlsUI extends ControlsUI<View, CollectorView> implements AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener
{
	
	// Statics-------------------------------------------------------
	static public final float CONTROL_HEIGHT_DIP = 60.0f;
	static public final float PADDING_DIP = 6.0f;
	static public final int FOREGROUND_COLOR = Color.BLACK;
	static private final int SEMI_TRANSPARENT_WHITE = Color.parseColor("#80FFFFFF");
	
	// Dynamics------------------------------------------------------
	private ControlItem[] controlItems;
	private PickerView view;
	private CollectorController controller;
	
	public AndroidControlsUI(CollectorController controller, CollectorView collectorView)
	{
		super(controller, collectorView);

		this.controller = controller;
		
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
			view.setOnItemLongClickListener(this);
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
		view.setRecycleViews(!newForm.isClickAnimation());
		
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
		if(controller.getCurrentForm().isClickAnimation())
			(new ClickAnimator(action, v, collectorUI)).execute(); //execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)
	}
	
	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View v, int position, long id)
	{
		// Get the pressed ControlItem
		ControlItem item = (ControlItem) view.getAdapter().getItem(position);

		// Check whether AudioFeedback is supported for the current form
		AudioFeedback audioFeedback = controller.getCurrentForm().getAudioFeedback();

		if(audioFeedback != null)
		{
			switch(audioFeedback)
			{
			case LONG_CLICK_AUDIO_FILES:
			case SEQUENTIAL_AUDIO_FILES:

				// If the choice has an audio, pass that audio to the Media Player
				// TODO
				return true;

			case LONG_CLICK_TTS:
			case SEQUENTIAL_TTS:

				// Enable TTS Audio Feedback
				controller.textToVoice(item.getDescription());
				break;

			case NONE:
				controller.addLogLine("LONG_CLICK", "LongClick on " + item.getDescription() + " but AudioFeedback is disabled");
				return true;
			}

			// Apply an alpha animation to the long pressed view
			ViewAnimator.shakeAnimation(v);
		}

		return true;
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
					this.setDescription(form.getBackButtonDescription());
					break;
				case CANCEL:
					imgRelativePath = form.getCancelButtonImageRelativePath();
					drawable = new SaltireCross(FOREGROUND_COLOR);
					this.setDescription(form.getCancelButtonDescription());
					break;
				case FORWARD:
					imgRelativePath = form.getForwardButtonImageRelativePath();
					drawable = new HorizontalArrow(FOREGROUND_COLOR, false);
					this.setDescription(form.getForwardButtonDescription());
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
