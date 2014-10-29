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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.TextItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * The UI for ChoiceFields
 * 
 * @author mstevens
 */
public class AndroidChoiceUI extends ChoiceUI<View, CollectorView>
{
	
	static public final float PAGE_CHOSEN_ITEM_SIZE_DIP = 60.0f; // width = height
	static public final float PAGE_CHOSEN_ITEM_MARGIN_DIP = 1.0f; // same margin all round
	static public final float CROSS_THICKNESS = 0.02f;
	
	private PageView pageView;
	private ChoiceView choiceView;

	private CollectorController controller;
	private AudioFeedbackController audioController;
	private ChoiceField choice;

	public AndroidChoiceUI(ChoiceField choice, CollectorController controller, CollectorView collectorView)
	{
		super(choice, controller, collectorView);
		
		this.controller = controller;
		this.audioController = controller.getAudioFeedbackController();
		this.choice = choice;
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(onPage || !enabled)
		{	// on page or disabled:
			if(!field.isRoot())
				return null; // just in case
			
			if(pageView == null)
				pageView = new PageView(collectorUI.getContext());
			
			// Enable/disable (do this before calling setChosen()!):
			pageView.setEnabled(enabled); // also sets up event listeners!
			
			// Update pageView:
			ChoiceField chosen = field.getSelectedChoice(record);
			if(chosen != null)
				pageView.setChosen(chosen);
			else
				pageView.setChosen(field);
			
			return pageView;
		}
		else
		{	// not on page and enabled:
			if(choiceView == null)
				choiceView = getChoiceView();
			
			// Update & enable pickView:
			choiceView.update();
			choiceView.setEnabled(true);
			
			return (View) choiceView;
		}
	}
	
	protected void onChildClick(final ChoiceField child, View childView)
	{
		// Ignore click if child is disabled:
		if(!isFieldShown() && !controller.isFieldEnabled(child))
			return;

		// Task to perform after animation has finished:
		Runnable action = new Runnable()
		{
			public void run()
			{
				choiceMade(child);
			}
		};

		// Execute the "press" animation if allowed, then perform the action: 
		if(controller.getCurrentForm().isClickAnimation())
			(new ClickAnimator(action, childView, controller)).execute(); // execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)	
	}
	
	protected boolean onChildLongClick(Context context, final ChoiceField child, View childView)
	{
		// Ignore click if child is disabled:
		if(!isFieldShown() && !controller.isFieldEnabled(child))
			return false;

		// Audio Feedback
		audioController.playAnswer(context, child, childView);

		return true;
	}
	
	@Override
	public void cancel() {
		controller.stopAudioFeedback();
	}

	/**
	 * To be overridden by AndroidICSChoiceUI
	 * 
	 * @return
	 */
	public ChoiceView getChoiceView()
	{
		return new PreICSChoiceView(collectorUI.getContext());
	}
	
	/**
	 * View for displaying a ChoiceField as part of a Page
	 * 
	 * @author mstevens
	 */
	public class PageView extends LinearLayout implements OnClickListener, OnFocusChangeListener
	{

		private TextView label;
		private View chosenView;
		private int chosenSizePx;
		private int chosenPaddingPx;
		private int chosenMarginPx;
		
		public PageView(Context context)
		{
			super(context);
			this.setOrientation(LinearLayout.VERTICAL);
			
			// Add label:
			label = new TextView(getContext());
			label.setText(field.getCaption());
			this.addView(label);
			
			chosenSizePx = ScreenMetrics.ConvertDipToPx(context, PAGE_CHOSEN_ITEM_SIZE_DIP);
			chosenPaddingPx = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
			chosenMarginPx = ScreenMetrics.ConvertDipToPx(context, PAGE_CHOSEN_ITEM_MARGIN_DIP);
		}
		
		public void setChosen(ChoiceField chosenField)
		{
			// Remove previous:
			if(chosenView != null)
				this.removeView(chosenView);
			
			// New chosenView
			chosenView = createItem(chosenField, chosenPaddingPx, !isEnabled()).getView(getContext());
			
			// Set margins on layoutparams:
			LayoutParams chosenLP = new LinearLayout.LayoutParams(chosenSizePx, chosenSizePx);
			chosenLP.setMargins(chosenMarginPx, chosenMarginPx, chosenMarginPx, chosenMarginPx);
			
			// Add the view:
			this.addView(chosenView, chosenLP);
			
			// Enable/disable the chosenView:
			setEnabled(isEnabled());
		}
		
		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			if(chosenView != null)
			{
				chosenView.setEnabled(enabled);
				chosenView.setOnClickListener(enabled ? this : null);
				// Make other fields lose focus and simulate clicking with onFocusChange:
				chosenView.setFocusable(enabled);
				chosenView.setFocusableInTouchMode(enabled);
				chosenView.setOnFocusChangeListener(enabled ? this : null);
			}
		}

		@Override
		public void onFocusChange(View v, boolean hasFocus)
		{
			if(hasFocus && isFieldShown() && isEnabled() && v.isEnabled())
			{
				// Lose focus again:
				v.clearFocus();
				
				// Simulate click:
				onClick(v);
			}
		}

		@Override
		public void onClick(View v)
		{
			// Do nothing if not shown or enabled:
			if(!isFieldShown() || !isEnabled() || !v.isEnabled())
				return;
			
			// The user will make a choice now, so don't annoy him/her with the red box:
			clearPageInvalidMark();
			
			// Task to perform after animation has finished:
			Runnable action = new Runnable()
			{
				public void run()
				{
					controller.goTo(new FieldWithArguments(field), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the field itself
				}
			};

			// Execute the "press" animation if allowed, then perform the action: 
			if(controller.getCurrentForm().isClickAnimation())
				(new ClickAnimator(action, v, controller)).execute(); // execute animation and the action afterwards
			else
				action.run(); //perform task now (animation is disabled)			
		}
		
	}
	
	/**
	 * ChoiceView interface
	 * 
	 * @author mstevens
	 */
	public interface ChoiceView
	{
		
		public void update();
		
		public void setEnabled(boolean enabled);
		
		public View getChildViewAt(int index);
		
	}
	
	/**
	 * 
	 * @author Julia, mstevens, Michalis Vitos
	 */
	private class PreICSChoiceView extends PickerView implements ChoiceView, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener
	{
						
		public PreICSChoiceView(Context context)
		{
			super(context);
			
			// UI set-up:
			setBackgroundColor(Color.BLACK);
			int spacingPx = collectorUI.getSpacingPx();
			setHorizontalSpacing(spacingPx);
			setVerticalSpacing(spacingPx);
			
			// Number of columns:
			setNumColumns(field.getCols());
			
			// Item size & padding:
			setItemDimensionsPx(collectorUI.getFieldUIPartWidthPx(field.getCols()),
								collectorUI.getFieldUIPartHeightPx(field.getRows()));
			int itemPaddingPx = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);

			// Add items for children:
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
				adapter.addItem(createItem(child, itemPaddingPx, !controller.isFieldEnabled(child)));
			// Click listeners:
			setOnItemClickListener(this);
			setOnItemLongClickListener(this);
			
			// Only begin audio feedback once the system calls back to tell us all the views have been drawn:
			getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {

				@Override
                public void onGlobalLayout() {
					// when the views have all been created, play the question
					audioController.playQuestion(collectorUI.getContext(), choice, PreICSChoiceView.this);
					// remove this listener once this has occurred, since the question will only be played once
					PreICSChoiceView.this.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                }
				
			});
		}
		
		public void update()
		{
			// Update grayed-out state:
			int c = 0;
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
				adapter.getItem(c++).setVisibility(controller.isFieldEnabled(child));
			setAdapter(adapter);
		}
		
		public View getChildViewAt(int index) {
			return this.getChildAt(index);
		}
		
		@Override
		public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
		{
			if(isEnabled())
				onChildClick(field.getChildren().get(position) /* pass the chosen child */, v);
		}
		
		@Override
		public boolean onItemLongClick(AdapterView<?> parent, View v, int position, long id)
		{
			if(isEnabled())
				return onChildLongClick(getContext(), field.getChildren().get(position) /* pass the chosen child */, v);

			return false;
		}

	}
	
	/**
	 * Creates an DictionaryItem object responding to the provided child ChoiceField
	 * 
	 * Note: if we add colSpan/rowSpan support the right itemWidth/Height would need to be computed here (e.g.: for rowSpan=2 the itemWidth becomes (itemWidth*2)+spacingPx)
	 * 
	 * @param child
	 * @return corresponding item
	 */
	/**
	 * @param child
	 * @param itemPaddingPx
	 * @param grayedOut
	 * @return
	 */
	public Item createItem(ChoiceField child, int itemPaddingPx, boolean grayedOut)
	{
		File imageFile = controller.getProject().getImageFile(controller.getFileStorageProvider(), child.getImageRelativePath());
		Item item = null;
		if(FileHelpers.isReadableFile(imageFile))
			item = new FileImageItem(imageFile);
		else
			item = new TextItem(child.getAltText()); //render alt text instead of image
		
		// Set background colour:
		item.setBackgroundColor(ColourHelpers.ParseColour(child.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		
		// Crossing & graying out
		if(child.isCrossed() || grayedOut)
		{	
			LayeredItem layeredItem = new LayeredItem();
			layeredItem.addLayer(item, false);
			// Crossing:
			if(child.isCrossed())
				layeredItem.addLayer(new DrawableItem(new SaltireCross(ColourHelpers.ParseColour(child.getCrossColor(), ChoiceField.DEFAULT_CROSS_COLOR), CROSS_THICKNESS))); // later we may expose thickness in the XML as well
			// Graying-out:
			if(grayedOut)
			{
				// Make background of layered stack gray:
				layeredItem.setBackgroundColor(CollectorView.COLOR_GRAY);
				// Add grayed-out layer:
				Item grayOutOverlay = new EmptyItem();
				grayOutOverlay.setBackgroundColor(CollectorView.COLOR_SEMI_TRANSPARENT_GRAY);
				layeredItem.addLayer(grayOutOverlay, false);	
			}
			// Item becomes layered:
			item = layeredItem;
		}
		
		// Set size & padding:
		item.setPaddingPx(itemPaddingPx);
		
		// Set the description used for accessibility support
		item.setDescription(child.getAltText());

		return item;
	}
	
}