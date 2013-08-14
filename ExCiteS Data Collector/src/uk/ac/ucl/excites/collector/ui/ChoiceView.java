package uk.ac.ucl.excites.collector.ui;

import java.io.File;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.ui.picker.ImageFileItem;
import uk.ac.ucl.excites.collector.ui.picker.PickerAdapter;
import uk.ac.ucl.excites.collector.ui.picker.PickerView;
import uk.ac.ucl.excites.collector.ui.picker.PlaceholderItem;
import uk.ac.ucl.excites.collector.ui.picker.TextItem;
import uk.ac.ucl.excites.util.Debug;
import uk.ac.ucl.excites.util.FileHelpers;
import android.content.Context;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;

/**
 * @author Julia, mstevens, Michalis Vitos
 * 
 */
public class ChoiceView extends PickerView implements FieldView
{
	private Context context;

	public ChoiceView(Context context)
	{
		super(context);
		this.context = context;
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
				pickerAdapter.setItemWidth((getWidth() - ((choice.getCols() - 1) * getSpacingInDp(context))) / choice.getCols());
				pickerAdapter.setItemHeight((getHeight() - ((choice.getRows() - 1) * getSpacingInDp(context))) / choice.getRows());
				setAdapter(pickerAdapter);
				
				getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});

		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, final int position, long id)
			{
				// Check if the UI is waiting for animation, if it does do nothing
				if(!CollectorActivity.isWaitingForUIAnimation())
				{
					Debug.d("ChoiceView is clicked and NOT waiting for UI, the pressed code is executed.");

					// Run the animation
					Runnable task = new Runnable()
					{
						public void run()
						{
							ChoiceField chosenChild = choice.getChildren().get(position);
							if(controller.isFieldEndabled(chosenChild))
								controller.choiceMade(chosenChild); // pass the chosen child
						}
					};

					Animator animator = new Animator(task, v);
					animator.execute();

				}
				else
				{
					Debug.d("ChoiceView is clicked but is waiting for UI.");
				}
			}
		});

		setOnItemLongClickListener(new OnItemLongClickListener()
		{
			@Override
			public boolean onItemLongClick(AdapterView<?> arg0, View v, int arg2, long arg3)
			{
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