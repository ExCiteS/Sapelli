package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import android.content.Context;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;

public class ChoiceView extends PickerView implements AdapterView.OnItemClickListener
{

	private ProjectController controller;
	private Choice currentChoice;	
	
	public ChoiceView(Context context)
	{
		super(context);
		setOnItemClickListener(this); //Set-up click listener
	}

	public void setChoice(final Choice choice, ProjectController controller)
	{
		if(choice.isLeaf())
			throw new IllegalArgumentException("Cannot display leaf choice.");
		this.currentChoice = choice;
		this.controller = controller;
		
		// Number of columns:
		setNumColumns(choice.getCols());
		
		// Adapter & images:
		imageAdapter = new ImageAdapter(getContext());
		for(Choice child : choice.getChildren())
			imageAdapter.addImage(new FileImage(controller.getProject(), child.getImageLogicalPath()));
		
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
	}
	
	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id)
	{
		controller.choiceMade(currentChoice.getChildren().get(position)); //pass the chosen child
	}

}