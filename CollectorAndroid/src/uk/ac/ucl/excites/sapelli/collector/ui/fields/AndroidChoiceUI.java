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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.ItemPickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.TextFitView.TextSizeCoordinator;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ItemHelpers;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.MeasureItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.SplitItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.TextItem;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.ColourHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
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

	@SuppressWarnings("unused")
	static private final String TAG = "AndroidChoiceUI";
	
	static public final float PAGE_CHOSEN_ITEM_SIZE_DIP = 60.0f; // width = height
	static public final float PAGE_CHOSEN_ITEM_MARGIN_DIP = 1.0f; // same margin all round
	static public final float CROSS_THICKNESS = 0.02f;
	
	static public final int SPLIT_ITEM_CHILD_PADDING_DIP = 0;
	
	private PageView pageView;
	private ChoiceView choiceView;

	public AndroidChoiceUI(ChoiceField choice, CollectorController controller, CollectorView collectorView)
	{
		super(choice, controller, collectorView);
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

		// Perform the click
		collectorUI.clickView(childView, action);
	}
	
	protected boolean onChildLongClick(Context context, final ChoiceField child, View childView)
	{
		// Ignore click if child is disabled:
		if(!isFieldShown() && !controller.isFieldEnabled(child))
			return false;

		// check if both the form and the field use audio feedback, and make sure were not given a root choice (which doesn't have an answersDescription)
		if(isUsingAudioFeedback(false) && !child.isRoot())
		{
			AudioFeedbackController<View> afc = collectorUI.getAudioFeebackController();
			afc.play(afc.newPlaybackJob(child.getAnswerDescription().getAudioRelativePath(), childView, AudioFeedbackController.ANIMATION_SHAKE));
		}
		else
			controller.addLogLine("LONG_CLICK", "LongClick on " + child.toString(false) + " but AudioFeedback is disabled");

		return true;
	}

	/**
	 * To be overridden by AndroidICSChoiceUI
	 * 
	 * @return a View object (not type checked!) of a class that implements ChoiceView
	 */
	protected ChoiceView getChoiceView()
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
			if(field.hasCaption())
			{
				label = new TextView(getContext());
				
				// Ensure that the label text is not truncated, by setting width to WRAP_CONTENT:
				label.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
				label.setText(field.getCaption());
				
				this.addView(label);
			}
			
			chosenSizePx = ScreenMetrics.ConvertDipToPx(context, PAGE_CHOSEN_ITEM_SIZE_DIP);
			chosenPaddingPx = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
			chosenMarginPx = ScreenMetrics.ConvertDipToPx(context, PAGE_CHOSEN_ITEM_MARGIN_DIP);
			
			// Set the description used for accessibility support:
			setContentDescription(field.getCaption());
		}
		
		public void setChosen(ChoiceField chosenField)
		{
			// Remove previous:
			if(chosenView != null)
				this.removeView(chosenView);
			
			// New chosenView
			chosenView = createItem(chosenField, chosenPaddingPx, !isEnabled(), null, null, null).getView(getContext());
			
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

			// Perform the click
			collectorUI.clickView(v, action);
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
		
		/**
		 * @param index of a choice child
		 * @return the view representing that choice
		 */
		public View getChildAt(int index);
		
	}
	
	/**
	 * 
	 * @author Julia, mstevens, Michalis Vitos
	 */
	private class PreICSChoiceView extends ItemPickerView implements ChoiceView, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener
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
			
			// Item size:
			setItemDimensionsPx(collectorUI.getFieldUIPartWidthPx(field.getCols()),
								collectorUI.getFieldUIPartHeightPx(field.getRows()));
			
			// Text size coordinators:
			TextSizeCoordinator textOnlyCoordinator = field.isMatchTextSize() ? new TextSizeCoordinator() : null;
			TextSizeCoordinator captionCoordinator = field.isMatchTextSize() ? new TextSizeCoordinator() : null;
			TextSizeCoordinator measureCoordinator = field.form.isShowImageSizes() ? new TextSizeCoordinator() : null;
			
			// Add items for children:
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
				adapter.addItem(createItem(child, CollectorView.PADDING_DIP, !controller.isFieldEnabled(child), textOnlyCoordinator, captionCoordinator, measureCoordinator));
			
			// Click listeners:
			setOnItemClickListener(this);
			if(isUsingAudioFeedback(false))
				setOnItemLongClickListener(this);
			
			// Set the description used for accessibility support:
			setContentDescription(field.getQuestionDescription().getText());
		}
		
		@Override
		public void update()
		{
			// Update grayed-out state:
			int c = 0;
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
			{
				Item<?> childItem = adapter.getItem(c++);
				if(childItem != null)
					childItem.setVisibility(controller.isFieldEnabled(child));
			}
			setAdapter(adapter);
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
	 * Creates an Item object responding to the provided (child) ChoiceField
	 * 
	 * @param choice
	 * @param itemPaddingDip
	 * @param grayedOut
	 * @param textOnlyCoordinator
	 * @param captionCoordinator
	 * @param measureCoordinator coordinator for text size of displayed image measurements, or null if no image measurements should be shown (so measureCoordinator != null is equivalent to choice.isShowImgSize())
	 * @return
	 */
	public Item<?> createItem(ChoiceField choice, float itemPaddingDip, boolean grayedOut, TextSizeCoordinator textOnlyCoordinator, TextSizeCoordinator captionCoordinator, TextSizeCoordinator measureCoordinator)
	{
		/* Note
		 * 	If the choice is the root it means we are on a page (meaning the item will be
		 * 	displayed together with caption-label above it) and there is no value yet.
		 * 	In this case we never show both an image and a caption (due to limited space),
		 * 	we also avoid repeating the caption which is already displayed above the item. */
		
		// Determine background colour:
		int bgColor = grayedOut ? CollectorView.COLOR_GRAY : ColourHelpers.ParseColour(choice.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR);
		
		Item<?> item = null;
		// Decide on appearance and get appropriate item(s):
		if(choice.getImageRelativePath() != null && choice.getCaptionHeight() < 1)
		{	// the is an image path (but not necessarily an accessible file!) and the caption does not take up the full height
			if(choice.hasCaption() && choice.getCaptionHeight() > 0 && !choice.isRoot())
			{	// there is a caption, a non-zero caption height & the choice not the root --> IMAGE + CAPTION:
				item = new SplitItem(SplitItem.VERTICAL).setSpacingDip(itemPaddingDip) // use same amount of spacing between split children as the outer item padding
					// add item for image (take up all space not taken up by caption):
					.addItem(	createImageItem(choice, false, textOnlyCoordinator, measureCoordinator)
									.setPaddingDip(SPLIT_ITEM_CHILD_PADDING_DIP) // 0 dip
									.setBackgroundColor(bgColor),
								1.0f - choice.getCaptionHeight())
					// add item for caption:
					.addItem(	createCaptionItem(choice, true, captionCoordinator, null)
									.setPaddingDip(SPLIT_ITEM_CHILD_PADDING_DIP) // 0 dip
									.setBackgroundColor(bgColor),
								choice.getCaptionHeight());
			}
			else
			{	// there is no caption, or its height is 0, or we are dealing with the root --> IMAGE ONLY
				item = createImageItem(choice, !choice.isRoot(), textOnlyCoordinator, measureCoordinator);
			}
		}
		else
		{	// there is no image path, or the caption takes up the full height --> CAPTION ONLY
			item = createCaptionItem(choice, !choice.isRoot(), textOnlyCoordinator, measureCoordinator); // regardless of the actual captionHeight the caption will take up the fill available height
		}

		// Crossing:
		if(choice.isCrossed())
		{
			// Make LayedItem and add item as bottom layer:
			LayeredItem layeredItem = new LayeredItem().addLayer(item, Color.TRANSPARENT, 0.0f); // make inner item background transparent and remove its padding
			// Add cross on top:
			layeredItem.addLayer(new DrawableItem(new SaltireCross(ColourHelpers.ParseColour(choice.getCrossColor(), ChoiceField.DEFAULT_CROSS_COLOR), CROSS_THICKNESS)), Color.TRANSPARENT, 0.0f); // later we may expose thickness in the XML as well
			// Item becomes layered:
			item = layeredItem;
		}
		
		// Set background colour (possibly gray):
		item.setBackgroundColor(bgColor);
		
		// Set padding:
		item.setPaddingDip(itemPaddingDip);
		
		// Graying-out (best to do this after item bgColour & padding have been set):
		if(grayedOut)
			item = ItemHelpers.GrayOut(item);
		
		// Set the answer description used for accessibility support
		item.setDescription(getAnswerDescriptionText(choice)); // has fall-backs & returns null for roots

		// Return the item:
		return item;
	}
	
	/**
	 * Returns an Item showing the image of the choice, or its alt text if the image file could not be found
	 * 
	 * @param choice
	 * @param standAlone whether the item will be displayed on its own, or not (i.e. under an image or under a page caption-label)
	 * @param coordinator for alt text size
	 * @param measureCoordinator coordinator for text size of displayed image measurements, or null if no image measurements should be shown
	 * @return
	 */
	private Item<?> createImageItem(ChoiceField choice, boolean standAlone, TextSizeCoordinator coordinator, TextSizeCoordinator measureCoordinator)
	{
		// Make image (or text) item:
		Item<?> item;
		File imageFile = controller.getFileStorageProvider().getProjectImageFile(field.form.project, choice.getImageRelativePath());
		if(FileHelpers.isReadableFile(imageFile))
			// render image:
			item = new FileImageItem(imageFile);
		else
		{	// render "alt" text instead of image:
			String alt = getAltText(choice, standAlone);
			// We make text color red if the alt text is the image path (meaning we couldn't display a caption or value String):
			item = new TextItem(alt, alt.equals(choice.getImageRelativePath()) ? Color.RED : TextItem.DEFAULT_TEXT_COLOR, coordinator);
		}
		
		// Return item, possibly with measurement showing:
		if(measureCoordinator != null)
			return MeasureItem.Measure(item, measureCoordinator);
		else
			return item;
	}

	/**
	 * Returns an Item which renders the render caption text.
	 * 
	 * @param child
	 * @param allowCaption whether the ChoiceField.caption can be used (because it is isn't already displayed above the caption item)
	 * @param coordinator for caption text size
	 * @param measureCoordinator coordinator for text size of displayed image measurements, or null if no image measurements should be shown
	 * @return
	 */
	private Item<?> createCaptionItem(ChoiceField child, boolean allowCaption, TextSizeCoordinator coordinator, TextSizeCoordinator measureCoordinator)
	{
		// Make text item:
		Item<?> item = new TextItem(getCaptionText(child, allowCaption), coordinator);
		
		// Return item, possibly with measurement showing:
		if(measureCoordinator != null)
			return MeasureItem.Measure(item, measureCoordinator);
		else
			return item;
	}

	@Override
	protected List<AudioFeedbackController<View>.PlaybackJob> getAudioFeedbackJobs(AudioFeedback audioFeedbackMode, boolean withPage)
	{
		switch(audioFeedbackMode)
		{
			case LONG_CLICK:
				// Just play question description when entering choice field:
				return Collections.singletonList(collectorUI.getAudioFeebackController().newPlaybackJob(field.getQuestionDescription().getAudioRelativePath()));
	
			case SEQUENTIAL:
				// Create a playlist that includes firstly the question description and then each answer description:
				List<AudioFeedbackController<View>.PlaybackJob> playlist = new ArrayList<AudioFeedbackController<View>.PlaybackJob>();
				AudioFeedbackController<View> afc = collectorUI.getAudioFeebackController();
				// Enqueue question description:
				playlist.add(afc.newPlaybackJob(field.getQuestionDescription().getAudioRelativePath()));
				// Enqueue answer description for each child:
				int c = 0;
				for(ChoiceField child : field.getChildren())
					playlist.add(afc.newPlaybackJob(child.getAnswerDescription().getAudioRelativePath(), choiceView.getChildAt(c++), AudioFeedbackController.ANIMATION_SHAKE));
				// Return playlist:
				return playlist;
	
			default:
				// Should never get here since this method is only called if audio feedback is enabled
				return Collections.<AudioFeedbackController<View>.PlaybackJob> emptyList();
		}
	}
	
}