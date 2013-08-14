package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.CollectorActivity;
import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.PlaceholderImage;
import uk.ac.ucl.excites.util.Debug;
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
		imageAdapter = new ImageAdapter(getContext());
		boolean atLeastOneEnabledChild = false;
		for(ChoiceField child : choice.getChildren())
		{
			if(controller.isFieldEndabled(child))
			{
				imageAdapter.addImage(new FileImage(controller.getProject(), child.getImageLogicalPath()));
				atLeastOneEnabledChild = true;
			}
			else
				imageAdapter.addImage(new PlaceholderImage()); // show blank space instead of image for disabled choices
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
				imageAdapter.setImageWidth((getWidth() - ((choice.getCols() - 1) * getSpacingInDp(context))) / choice.getCols());
				imageAdapter.setImageHeight((getHeight() - ((choice.getRows() - 1) * getSpacingInDp(context))) / choice.getRows());
				setAdapter(imageAdapter);

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