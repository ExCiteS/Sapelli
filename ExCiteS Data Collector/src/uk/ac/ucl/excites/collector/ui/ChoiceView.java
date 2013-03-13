package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.PlaceholderImage;
import android.content.Context;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;

/**
 * @author Julia, mstevens
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
				imageAdapter.addImage(new PlaceholderImage()); //show blank space instead of image for disabled choices 
		}
		if(!atLeastOneEnabledChild)
		{	//all children are disabled
			controller.goForward(); //skip this field
			return;
		}
		
		// Set image dimensions when view dimensions are known:
		getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				imageAdapter.setImageWidth((getWidth() - ((choice.getCols() - 1) * SPACING )) / choice.getCols());
				imageAdapter.setImageHeight((getHeight() - ((choice.getRows() - 1) * SPACING)) / choice.getRows());
				setAdapter(imageAdapter);
				
				getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});
		
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				ChoiceField chosenChild = choice.getChildren().get(position);
				if(controller.isFieldEndabled(chosenChild))
					controller.choiceMade(chosenChild); //pass the chosen child
			}
		});
	}

	@Override
	public void cancel()
	{
		//does nothing
	}
	
}