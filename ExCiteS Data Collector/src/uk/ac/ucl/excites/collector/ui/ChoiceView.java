package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
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
		
		int imageWidth = (getWidth() - ((choice.getCols() - 1) * SPACING )) / choice.getCols();
		int imageHeight = (getHeight() - ((choice.getRows() - 1) * SPACING)) / choice.getRows();

		imageAdapter = new ImageAdapter(super.getContext(), controller.getProject(), imageWidth, imageHeight);
		imageAdapter.IconsToDisplay(choice.getChildren());
		setNumColumns(choice.getCols());
		setAdapter(imageAdapter);

		// set click listener
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