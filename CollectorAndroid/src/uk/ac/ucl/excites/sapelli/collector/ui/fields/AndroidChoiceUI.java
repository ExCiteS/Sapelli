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
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.media.AbstractAudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.media.AudioFeedbackController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form.AudioFeedback;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.OnPageView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
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
import android.widget.AdapterView;

/**
 * The UI for ChoiceFields
 * 
 * @author mstevens
 */
public class AndroidChoiceUI extends ChoiceUI<View, CollectorView>
{
	static public final float CROSS_THICKNESS = 0.02f;
	
	private ChoiceOnPageView pageView;
	private ChoiceView choiceView;

	private CollectorController controller;
	private AudioFeedbackController audioController;
	private ChoiceField choice;

	public AndroidChoiceUI(ChoiceField choice, CollectorController controller, CollectorView collectorView)
	{
		super(choice, controller, collectorView);
		
		this.controller = controller;
		this.audioController = new AudioFeedbackController(controller);
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
				pageView = new ChoiceOnPageView(collectorUI.getContext(), controller, this);
			
			// Update pageView:
			ChoiceField chosen = field.getSelectedChoice(record);
			if(chosen != null)
				pageView.setChosen(chosen);
			else
				pageView.setChosen(field);
			
			// Enable/disable (do this before calling setChosen()!): // TODO why before?!
			pageView.setEnabled(enabled); // also sets up event listeners!
			
			return pageView;
		}
		else
		{	// not on page and enabled:
			if(choiceView == null)
				choiceView = getChoiceView();
			
			// Update & enable pickView:
			choiceView.update();
			choiceView.setEnabled(true);
			
			// Audio Feedback
			audioController.playQuestion(choice);

			return (View) choiceView;
		}
	}
	
	protected void onChildClick(final ChoiceField child, View childView)
	{
		// Ignore click if child is disabled:
		if(!isFieldShown() && !controller.isFieldEnabled(child))
			return;
		
		// Stop the Audio Feedback
		controller.stopAudioFeedback();

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

		// Audio Feedback
		audioController.playAnswer(context, child, childView);

		return true;
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

	private class ChoiceOnPageView extends OnPageView<ChoiceField>
	{

		public ChoiceOnPageView(Context context, CollectorController controller, FieldUI<ChoiceField, View, CollectorView> fieldUi)
		{
			super(context, controller, fieldUi);
		}

		public void setChosen(ChoiceField chosenField)
		{
			// New chosenView
			View contentView = createItem(chosenField, 0, !isEnabled()).getView(getContext());
			// Enable/disable the chosenView:
			setEnabled(isEnabled());
			// Add the view:
			this.setContentView(contentView);
		}
	}

	/**
	 * ChoiceView interface
	 * 
	 * @author mstevens
	 */
	protected interface ChoiceView
	{
		
		public void update();
		
		public void setEnabled(boolean enabled);
		
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
		
		public void update()
		{
			// Update grayed-out state:
			int c = 0;
			PickerAdapter adapter = getAdapter();
			for(ChoiceField child : field.getChildren())
			{
				Item childItem = adapter.getItem(c++);
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

	@Override
	protected List<AbstractAudioFeedbackController<View>.PlaybackJob> getAudioFeedbackJobs(AudioFeedback audioFeedbackMode, boolean withPage)
	{
		// TODO Auto-generated method stub
		return null;
	}
	
}