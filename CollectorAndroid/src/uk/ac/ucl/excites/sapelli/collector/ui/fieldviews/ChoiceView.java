package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.control.ProjectController;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.PressAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.drawables.SaltireCross;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.DrawableItem;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.picker.items.TextItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.util.io.FileHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.AdapterView;

/**
 * The UI for ChoiceFields
 * 
 * @author Julia, mstevens, Michalis Vitos
 * 
 * TODO later we may implement colSpan/rowSpan support here, but that would require us to base the ChoiceView on GridLayout rather than PickerView/GridView
 */
@SuppressLint("ViewConstructor")
public class ChoiceView extends PickerView implements FieldUI, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener
{
	
	static public final String TAG = "ChoiceView";
	
	static public final float PADDING_DIP = 2.0f;
	static public final float CROSS_THICKNESS = 0.02f;
	
	private int itemWidthPx = Item.DEFAULT_WIDTH_PX;
	private int itemHeightPx = Item.DEFAULT_HEIGHT_PX;
	private int itemPaddingPx = Item.DEFAULT_PADDING_PX;
	
	private CollectorView collectorView;
	
	private final ProjectController controller;
	private final ChoiceField choice;
	
	public ChoiceView(Context context, CollectorView collectorView, ProjectController controller, ChoiceField choice)
	{
		super(context);
		if(choice.isLeaf()) // just in case...
			throw new IllegalArgumentException("Cannot display leaf choice.");
		this.collectorView = collectorView;
		this.controller = controller;
		this.choice = choice;
		
		// UI set-up:
		setBackgroundColor(Color.BLACK);
		int spacingPx = collectorView.getSpacingPx();
		setHorizontalSpacing(spacingPx);
		setVerticalSpacing(spacingPx);
		
		// Number of columns:
		setNumColumns(choice.getCols());
		
		// DictionaryItem size & padding:
		itemWidthPx = collectorView.getIconWidthPx(choice.getCols());
		itemHeightPx = collectorView.getIconHeightPx(choice.getRows(), controller.getControlsState().isAnyButtonShown());
		itemPaddingPx = ScreenMetrics.ConvertDipToPx(getContext(), PADDING_DIP);

		// Adapter & images:
		pickerAdapter = new PickerAdapter(getContext());
		int i = 0;
		for(ChoiceField child : choice.getChildren())
			pickerAdapter.addItem(createItem(child, ++i));
		
		// Click listeners:
		setOnItemClickListener(this);
		setOnItemLongClickListener(this);
	}
	
	/**
	 * Creates an DictionaryItem object responding to the provided child ChoiceField
	 * 
	 * Note: if we add colSpan/rowSpan support the right itemWidth/Height would need to be computed here (e.g.: for rowSpan=2 the itemWidth becomes (itemWidth*2)+spacingPx)
	 * 
	 * @param child
	 * @return corresponding item
	 */
	private Item createItem(ChoiceField child, int i)
	{
		File imageFile = controller.getProject().getImageFile(child.getImageRelativePath());
		Item item = null;
		if(FileHelpers.isReadableFile(imageFile))
			item = new FileImageItem(imageFile);
		else
			item = new TextItem(child.getAltText()); //render alt text instead of image
		
		// Crossing
		if(child.isCrossed())
		{
			LayeredItem crossedItem = new LayeredItem();
			int crossColour = ColourHelpers.ParseColour(child.getCrossColor(), ChoiceField.DEFAULT_CROSS_COLOR);
			crossedItem.addLayer(item);
			crossedItem.addLayer(new DrawableItem(new SaltireCross(crossColour, CROSS_THICKNESS))); // later we may expose thickness in the XML as well
			item = crossedItem;
		}
		
		// Set size & padding:
		item.setWidthPx(itemWidthPx);
		item.setHeightPx(itemHeightPx);
		item.setPaddingPx(itemPaddingPx);
		
		// Set background colour:
		item.setBackgroundColor(ColourHelpers.ParseColour(child.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		
		return item;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
	{
		// Task to perform after animation has finished:
		Runnable action = new Runnable()
		{
			public void run()
			{
				ChoiceField chosenChild = ChoiceView.this.choice.getChildren().get(position);
				if(ChoiceView.this.controller.isFieldEndabled(chosenChild))
					ChoiceView.this.controller.choiceMade(chosenChild); // pass the chosen child
			}
		};

		// Execute the "press" animation if allowed, then perform the action: 
		if(controller.getCurrentForm().isAnimation())
			(new PressAnimator(action, v, collectorView)).execute(); //execute animation and the action afterwards
		else
			action.run(); //perform task now (animation is disabled)
	}
	
	@Override
	public boolean onItemLongClick(AdapterView<?> arg0, View v, int position, long id)
	{
		return false;
	}

	@Override
	public void cancel()
	{
		// does nothing
	}

	@Override
	public Field getField()
	{
		return choice;
	}

	@Override
	public void update(Record record)
	{
		// Update visibility:
		int c = 0;
		for(ChoiceField child : choice.getChildren())
			pickerAdapter.getItem(c++).setVisibility(controller.isFieldEndabled(child));
		setAdapter(pickerAdapter);
	}

	@Override
	public boolean isValid(Record record)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeValue(Record record)
	{
		// TODO Auto-generated method stub
		
	}

}