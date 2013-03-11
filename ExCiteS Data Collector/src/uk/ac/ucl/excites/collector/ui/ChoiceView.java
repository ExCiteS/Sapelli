package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
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
public class ChoiceView extends PickerView
{

	public ChoiceView(Context context)
	{
		super(context);
	}

	public void setChoice(final Choice choice, final ProjectController controller)
	{
		if(choice.isLeaf())
			throw new IllegalArgumentException("Cannot display leaf choice.");		
		
		// Number of columns:
		setNumColumns(choice.getCols());
		
		// Adapter & images:
		imageAdapter = new ImageAdapter(getContext());
		boolean atLeastOneEnabledChild = false;
		for(Choice child : choice.getChildren())
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
				Choice chosenChild = choice.getChildren().get(position);
				if(controller.isFieldEndabled(chosenChild))
					controller.choiceMade(chosenChild); //pass the chosen child
			}
		});
	}

}