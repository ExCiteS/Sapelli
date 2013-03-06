package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.ui.images.FileImage;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import android.content.Context;
import android.view.View;
import android.widget.AdapterView;

public class ChoiceView extends PickerView
{
	public ChoiceView(Context context)
	{
		super(context);
	}

	private Choice currentChoice;

	public void setChoice(Choice choice, final ProjectController controller)
	{
		if(choice.isLeaf())
			throw new IllegalArgumentException("Cannot display leaf choice.");
		this.currentChoice = choice;
		Project project = controller.getProject();
		
		setNumColumns(choice.getCols());
		
		// Adapter & images:
		int imageWidth = (getWidth() - ((choice.getCols() - 1) * SPACING )) / choice.getCols();
		int imageHeight = (getHeight() - ((choice.getRows() - 1) * SPACING)) / choice.getRows();
		imageAdapter = new ImageAdapter(super.getContext(), imageWidth, imageHeight);
		for(Choice child : choice.getChildren())
			imageAdapter.addImage(new FileImage(project, child.getImageLogicalPath()));
		setAdapter(imageAdapter);

		// Set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				// get children of pressed icon or jump
				controller.choiceMade(currentChoice.getChildren().get(position));
			}
		});
	}

}