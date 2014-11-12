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
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.SplitItem;
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
	
	private PageView pageView;
	private ChoiceView choiceView;

	private CollectorController controller;
	private ChoiceField choice;

	public AndroidChoiceUI(ChoiceField choice, CollectorController controller, CollectorView collectorView)
	{
		super(choice, controller, collectorView);
		
		this.controller = controller;
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
			
			// Cast to View
			final View choiceViewView = (View) choiceView;
			

			
			return choiceViewView;
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
		controller.clickView(childView, action);
	}
	
	protected boolean onChildLongClick(Context context, final ChoiceField child, View childView)
	{
		// Ignore click if child is disabled:
		if(!isFieldShown() && !controller.isFieldEnabled(child))
			return false;

		if(isFieldUsingAudioFeedback(false)) {
			AudioFeedbackController<View> afc = collectorUI.getAudioFeebackController();
			afc.play(afc.new PlaybackJob(child.getAnswerDescriptionAudioRelativePath(), childView));
		}
		else
			controller.addLogLine("LONG_CLICK", "LongClick on " + choice.toString(false) + " but AudioFeedback is disabled");

		return true;
	}
	
	@Override
	public void cancel() {
		if (isFieldUsingAudioFeedback(false))
			collectorUI.stopAudioFeedback();
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
				//ensure that the label text is not truncated, by setting width to WRAP_CONTENT:
				label.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
				label.setText(field.getCaption());
				this.addView(label);
			}
			
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

			// Perform the click
			controller.clickView(v, action);
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
			if(isUsingAudioFeedback(false))
				setOnItemLongClickListener(this);
		}
		
		@Override
		public void update()
		{
			// Update grayed-out state:
			int c = 0;
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
				adapter.getItem(c++).setVisibility(controller.isFieldEnabled(child));
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
	 * @param itemPaddingPx
	 * @param grayedOut
	 * @return
	 */
	public Item createItem(ChoiceField choice, int itemPaddingPx, boolean grayedOut)
	{
		/* Note
		 * 	If the choice is the root it means we are on a page (meaning the item will be
		 * 	displayed together with caption-label above it) and there is no value yet.
		 * 	In this case we never show both an image and a caption (due to limited space),
		 * 	we also avoid repeating the caption which is already displayed above the item. */
		
		Item item = null;
		// Decide on appearance and get appropriate item(s):
		if(choice.getImageRelativePath() != null && choice.getCaptionHeight() < 1)
		{	// the is an image path (but not necessarily an accessible file) and the caption does not take up the full height
			if(choice.hasCaption() && choice.getCaptionHeight() > 0 && !choice.isRoot())
			{	// there is a caption, a non-zero caption height & the choice not the root --> IMAGE + CAPTION:
				item = new SplitItem(SplitItem.VERTICAL) // create new split item
					// add item for image (take up all space not taken up by caption):
					.addItem(createImageItem(choice, false), 1.0f - choice.getCaptionHeight())
					// add item for caption:
					.addItem(createCaptionItem(choice, false), choice.getCaptionHeight());
			}
			else
			{	// there is no caption, or its height is 0, or we are dealing with the root --> IMAGE ONLY
				item = createImageItem(choice, !choice.isRoot());
			}
		}
		else
		{	// there is no image path, or the caption takes up the full height --> CAPTION ONLY
			item = createCaptionItem(choice, !choice.isRoot()); // regardless of the actual captionHeight the caption will take up the fill available height
		}
		
		// Set background colour:
		item.setBackgroundColor(ColourHelpers.ParseColour(choice.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));

		// Crossing & graying out
		if(choice.isCrossed() || grayedOut)
		{
			LayeredItem layeredItem = new LayeredItem();
			layeredItem.addLayer(item, false);
			// Crossing:
			if(choice.isCrossed())
				layeredItem.addLayer(new DrawableItem(new SaltireCross(ColourHelpers.ParseColour(choice.getCrossColor(), ChoiceField.DEFAULT_CROSS_COLOR), CROSS_THICKNESS))); // later we may expose thickness in the XML as well
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
		item.setDescription(choice.getAnswerDescription()); // TODO fallbacks ? (question)desc? caption? (e.g. when on page)

		return item;
	}
	
	/**
	 * @param choice
	 * @param standAlone whether the text will be displayed on its own, or not (i.e. under an image or under a page caption-label)
	 * @return
	 */
	private Item createImageItem(ChoiceField choice, boolean standAlone)
	{
		File imageFile = controller.getFileStorageProvider().getProjectImageFile(field.form.project, choice.getImageRelativePath());
		if(FileHelpers.isReadableFile(imageFile))
			// render image:
			return new FileImageItem(imageFile);
		else
		{	// render "alt" text instead of image:
			String alt = getAltText(choice, standAlone);
			// We make text color red if the alt text is the image path (meaning we couldn't display a caption or value String):
			return new TextItem(alt, alt.equals(choice.getImageRelativePath()) ? Color.RED : TextItem.DEFAULT_TEXT_COLOR);
		}
	}
	
	/**
	 * @param child
	 * @param standAlone whether the text will be displayed on its own, or not (i.e. under an image or under a page caption-label)
	 * @return
	 */
	private Item createCaptionItem(ChoiceField child, boolean standAlone)
	{	// render caption text:
		return new TextItem(getCaptionText(child, standAlone));
	}

	@Override
	protected List<AudioFeedbackController<View>.PlaybackJob> getAudioFeedbackJobs(AudioFeedback audioFeedbackMode, boolean withPage)
	{
		List<AudioFeedbackController<View>.PlaybackJob> playlist;
		switch (audioFeedbackMode) {
		case LONG_CLICK:
			// just play question description when entering choice field:
			playlist = new ArrayList<AudioFeedbackController<View>.PlaybackJob>(); // do not use singletonList as list should be mutable
			playlist.add(collectorUI.getAudioFeebackController().new PlaybackJob(field.getAnswerDescriptionAudioRelativePath()));
			return playlist;
			
		case SEQUENTIAL:
			// create a playlist that includes firstly the question description and then each answer description:
			playlist = new ArrayList<AudioFeedbackController<View>.PlaybackJob>();
			// question description:
			playlist.add(collectorUI.getAudioFeebackController().new PlaybackJob(field.getDescriptionAudioRelativePath()));
			// answer descriptions:
			List<ChoiceField> children = field.getChildren();
			for (int i = 0; i < children.size(); i++) {
				// enqueue each answer:
				playlist.add(collectorUI.getAudioFeebackController().new PlaybackJob(children.get(i).getAnswerDescriptionAudioRelativePath(), choiceView.getChildAt(i)));
			}
			
			return playlist;
			
		default:
			// should never get here since this method is only called if audio feedback is enabled
			return null;
			
		}
	}
}