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
import uk.ac.ucl.excites.sapelli.collector.media.AndroidAudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Control;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.EmptyDrawable;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.HorizontalArrow;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.MeasureItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.graphics.Color;
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
	static public final float CONTROL_PADDING_DIP = 6.0f;
	static public final int FOREGROUND_COLOR = Color.BLACK;
	static private final int SEMI_TRANSPARENT_WHITE = Color.parseColor("#80FFFFFF");
	
	// Dynamics------------------------------------------------------
	private ControlItem[] controlItems;
	private ItemPickerView view;
	private CollectorController controller;
	
	public AndroidControlsUI(CollectorController controller, CollectorView collectorView)
	{
		super(controller, collectorView);
		
		this.controller = controller;
		// ControlItem array:
		this.controlItems = new ControlItem[Control.Type.values().length];
	}

	@Override
	protected View getPlatformView()
	{
		if(view == null)
		{
			view = new ItemPickerView(collectorUI.getContext());
			
			// UI set-up:
			view.setBackgroundColor(Color.BLACK);
			view.setHorizontalSpacing(collectorUI.getSpacingPx());
			view.setPadding(0, 0, 0, collectorUI.getSpacingPx()); // Bottom padding (to put spacing between buttons and view underneath)
			
			// ControlItem size:
			view.setItemDimensionsPx(LayoutParams.MATCH_PARENT, collectorUI.getControlHeightPx());
			
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
		
		// (Re)instantiate control items (i.e. buttons):
		for(Control.Type controlType : Control.Type.values())
			controlItems[controlType.ordinal()] = new ControlItem(collectorUI.getContext(), newForm.getControl(controlType));
	}
	
	@Override
	protected void updateControlStates(State[] newControlStates)
	{
		if(view == null)
			return;
		
		// Update shown controlItems:
		PickerAdapter adapter = view.getAdapter();
		adapter.clear();
		for(Control.Type controlType : Control.Type.values())
		{
			State state = newControlStates[controlType.ordinal()]; 
			if(state != State.HIDDEN)
			{
				controlItems[controlType.ordinal()].setGrayedOut(state == State.SHOWN_DISABLED);
				adapter.addItem(controlItems[controlType.ordinal()]);
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
				handleControlEvent(Control.Type.values()[(int) id], false);
			}
		};

		// Perform the click
		collectorUI.clickView(v, action);
	}
	
	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View v, int position, long id)
	{
		Control.Type controlType = Control.Type.values()[(int) id];
		Form form = controller.getCurrentForm();
		if(form.isUsingAudioFeedback())
		{	// Audio Feedback...
			AudioFeedbackController<View> afc = collectorUI.getAudioFeebackController();
			afc.play(afc.newPlaybackJob(form.getControl(controlType).description.getAudioRelativePath(), v, AndroidAudioFeedbackController.ANIMATION_ALPHA));
		}
		else
			controller.addLogLine("LONG_CLICK", "LongClick on " + controlType.name() + " but AudioFeedback is disabled");
		return true;
	}

	@Override
	public int getCurrentHeightPx()
	{
		return view == null ? 0 : (view.getAdapter().isEmpty() ? 0 : (collectorUI.getControlHeightPx() + collectorUI.getSpacingPx()));
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
		private Item<?> grayOutOverlay;
	
		public ControlItem(Context context, Control control)
		{
			// Pass control type ordinal as id:
			super(control.type.ordinal());
			
			// Background & padding:
			this.setBackgroundColor(ColourHelpers.ParseColour(control.getBackgroundColor(), Control.DEFAULT_BACKGROUND_COLOR));
			this.setPaddingDip(0);
			
			// The actual button:
			Item<?> button;
			File imgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), control.getImageRelativePath());
			if(FileHelpers.isReadableFile(imgFile))
				// Use XML specified image:
				button = new FileImageItem(imgFile);
			else
				// Use default drawable for control type:
				switch(control.type)
				{
					case Back:
						button = new DrawableItem(new HorizontalArrow(FOREGROUND_COLOR, true));
						break;
					case Cancel:
						button = new DrawableItem(new SaltireCross(FOREGROUND_COLOR));
						break;
					case Forward:
						button = new DrawableItem(new HorizontalArrow(FOREGROUND_COLOR, false));
						break;
					default : 
						button = new DrawableItem(new EmptyDrawable());
				}
			/* Unused -- replaced by Drawable buttons (arrow & cross)
			// Resource image (e.g. R.drawable.button_back_svg, .button_back, .button_delete_svg, .button_delete, .button_forward_svg, .button_forward)
			button = new ResourceImageItem(getContext().getResources(), R.drawable.button_back_svg); */
			button.setPaddingDip(CONTROL_PADDING_DIP);
			button.setBackgroundColor(Color.TRANSPARENT); // button itself should not have a background so we can see the one of the containing ControlItem/LayeredItem
			
			// Show button image size:
			if(control.form.isShowImageSizes())
				button = MeasureItem.Measure(button); // no need for a TextSizeCoordinator here because all controls have the same size
			
			// the overlay
			grayOutOverlay = new EmptyItem();
			grayOutOverlay.setPaddingDip(0);
			setGrayedOut(false);
			
			// add the layers:
			this.addLayer(button);
			this.addLayer(grayOutOverlay);
			
			// Set description:
			this.setDescription(control.description.getText());
		}
		
		public void setGrayedOut(boolean grayedOut)
		{
			grayOutOverlay.setBackgroundColor(grayedOut ? SEMI_TRANSPARENT_WHITE : Color.TRANSPARENT);
		}
		
	}

}
