package uk.ac.ucl.excites.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.ui.picker.ImageFileItem;
import uk.ac.ucl.excites.collector.ui.picker.Item;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.collector.ui.picker.PlaceholderItem;
import uk.ac.ucl.excites.collector.ui.picker.TextItem;
import uk.ac.ucl.excites.util.FileHelpers;
import android.content.Context;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;

/**
 * @author Julia, mstevens, Michalis Vitos
 * 
 */
public class ChoiceView extends PickerView implements FieldView
{

	public ChoiceView(Context context)
	{
		super(context);
	}

	@Override
	public View getView()
	{
		return this;
	}

	@Override
	public void initialise(final ProjectController controller, Field field)
	{
		final ChoiceField choice = (ChoiceField) field;
		if(choice.isLeaf())
			throw new IllegalArgumentException("Cannot display leaf choice.");

		// Number of columns:
		setNumColumns(choice.getCols());

		// Adapter & images:
		pickerAdapter = new PickerAdapter(getContext());
		boolean atLeastOneEnabledChild = false;
		for(ChoiceField child : choice.getChildren())
		{
			if(controller.isFieldEndabled(child))
			{
				File imageFile = controller.getProject().getImageFile(child.getImageRelativePath());
				if(FileHelpers.isReadableFile(imageFile))
					pickerAdapter.addItem(new ImageFileItem(imageFile));
				else
					pickerAdapter.addItem(new TextItem(child.getAltText())); //render alt text instead of image
				atLeastOneEnabledChild = true;
			}
			else
				pickerAdapter.addItem(new PlaceholderItem()); // show blank space instead of image for disabled choices
		}
		if(!atLeastOneEnabledChild)
		{ // all children are disabled
			controller.goForward(false); // skip this field
			return;
		}

		// Set image dimensions when view dimensions are known:
		getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				pickerAdapter.setItemWidth((getWidth() - ((choice.getCols() - 1) * SPACING)) / choice.getCols());
				pickerAdapter.setItemHeight((getHeight() - ((choice.getRows() - 1) * SPACING)) / choice.getRows());
				
//				int minSize = Float.MAX_EXPONENT;
//				for(Item item : pickerAdapter.getItems())
//					if(item instanceof TextItem)
						
				
				setAdapter(pickerAdapter);
				
				getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});

		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				v.startAnimation(AnimationUtils.loadAnimation(getContext(), R.layout.image_click_animation));

				ChoiceField chosenChild = choice.getChildren().get(position);
				if(controller.isFieldEndabled(chosenChild))
					controller.choiceMade(chosenChild); // pass the chosen child
			}
		});
		
		setOnItemLongClickListener(new OnItemLongClickListener()
		{
			@Override
			public boolean onItemLongClick(AdapterView<?> arg0, View v, int arg2, long arg3)
			{
				v.startAnimation(AnimationUtils.loadAnimation(getContext(), R.layout.image_click_animation));
				return false;
			}
		});
	}

	@Override
	public void cancel()
	{
		// does nothing
	}

}