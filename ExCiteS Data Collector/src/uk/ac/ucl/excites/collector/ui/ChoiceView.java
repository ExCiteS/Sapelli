package uk.ac.ucl.excites.collector.ui;

import java.io.File;
import java.util.List;
import java.util.Stack;

import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import android.content.Context;
import android.graphics.Color;
import android.os.Environment;
import android.view.KeyEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.GridView;

public class ChoiceView extends GridView
{

	private final int BUTTON = 153;
	private final int NoBUTTON = 135;

	Stack<Choice> jumps = new Stack<Choice>(); // holds the choices from which the jumps occurred

	Context context;
	ImageAdapter imageAdapter;
	Choice startChoice;
	Choice pointer;
	List<Choice> currentItems;

	public ChoiceView(final Context context)
	{
		super(context);
		this.context = context;
		setBackgroundColor(Color.BLACK);

		// run Parser
		String xmlFilePath = Environment.getExternalStorageDirectory() + "/ExCiteSImagePicker/" + "ExCiteSCollectorXML.xml"; // path needs to be stored/passed as variable
		File xmlFile = new File(xmlFilePath);
		if(!xmlFile.exists())
			throw new IllegalArgumentException("XML file not found (" + xmlFilePath + ").");
		ProjectParser parser = new ProjectParser();
		Project project = parser.parseProject(xmlFile);

		// set start choice
		Form currentForm = project.getForms().get(0);
		startChoice = (Choice) currentForm.getStart();
		currentItems = startChoice.getChildren();
		imageAdapter = new ImageAdapter(context, BUTTON);
		setChoice(currentItems, startChoice.getCols());

		//set click listener
		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{

				// get children of pressed icon or jump
				pointer = currentItems.get(position);
				Choice jump = null;
				if(currentItems.get(position).getChildren().size() != 0)
				{
					currentItems = pointer.getChildren();
				}
				else
				{
					jumps.push(pointer);
					while(jump == null)
					{
						jump = (Choice) pointer.getJump();
						pointer = pointer.getParent();
					}
					pointer = jump;
					currentItems = pointer.getChildren();
				}

				// set new choice
				imageAdapter = new ImageAdapter(context, NoBUTTON);
				setChoice(currentItems, pointer.getCols());
			}
		});
	}

	public void setChoice(List<Choice> items, int cols)
	{
		imageAdapter.clearSelectedIcons();
		this.currentItems = items;
		imageAdapter.IconsToDisplay(items);
		setNumColumns(cols);
		setHorizontalSpacing(10);
		setVerticalSpacing(10);
		setAdapter(imageAdapter);
	}

	public void backPressed()
	{
		if(pointer.getParent() == null)
		{
			pointer = jumps.pop();
		}
		pointer = pointer.getParent();
		if(pointer.equals(startChoice))
		{
			imageAdapter = new ImageAdapter(context, BUTTON);
		}
		setChoice(pointer.getChildren(), pointer.getCols());
	}

	@Override
	// until back button is implemented, back key is used
	public boolean onKeyDown(int keyCode, KeyEvent event)
	{
		switch(keyCode)
		{
		case KeyEvent.KEYCODE_BACK:
			backPressed();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

}
