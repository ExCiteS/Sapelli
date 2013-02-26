package uk.ac.ucl.excites.collector.ui;

import java.util.List;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Choice;
import android.content.Context;
import android.graphics.Color;
import android.graphics.Point;
import android.util.Log;
import android.view.View;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.widget.AdapterView;
import android.widget.GridView;

public class ChoiceView extends GridView
{
	static private final int SPACING = 10;

	private ImageAdapter imageAdapter;
	private Choice currentChoice;

	public ChoiceView(final Context context)
	{
		super(context);

		// UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(10);
		setVerticalSpacing(10);
		setAdapter(imageAdapter);
	}

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