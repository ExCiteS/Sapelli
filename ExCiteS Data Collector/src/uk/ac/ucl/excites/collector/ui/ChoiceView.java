package uk.ac.ucl.excites.collector.ui;

import java.util.List;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.AdapterView;
import android.widget.GridView;

public class ChoiceView extends GridView
{
	static private final int BUTTON = 153;
	static private final int NO_BUTTON = 135;

	private ImageAdapter imageAdapter;
	private List<Choice> currentItems;


	public ChoiceView(final Context context)
	{
		super(context);
		
		//UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(10);
		setVerticalSpacing(10);
		setAdapter(imageAdapter);
	}

	public void setChoice(Choice choice, final ProjectController controller)
	{
		if(choice.isLeaf())
			throw new IllegalArgumentException("Cannot display leaf choice.");
		currentItems = choice.getChildren();
		
		if(choice.isRoot())
			imageAdapter = new ImageAdapter(super.getContext(), BUTTON);
		else
			imageAdapter = new ImageAdapter(super.getContext(), NO_BUTTON);
		
		imageAdapter.clearSelectedIcons();
		imageAdapter.IconsToDisplay(currentItems);
		setNumColumns(choice.getCols());
		setAdapter(imageAdapter);
		
		//set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				//get children of pressed icon or jump
				controller.choiceMade(currentItems.get(position));

			}
		});
	}

}
