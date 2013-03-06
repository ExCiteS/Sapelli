/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import android.content.Context;
import android.util.TypedValue;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia
 * 
 */
public class ButtonView extends PickerView
{
	public static float BUTTONHEIGHT = 45;

	public ButtonView(Context context)
	{
		super(context);
	}

	public void setButtonView(final ProjectController controller, final boolean showCancel, final boolean showBack, boolean showForward)
	{

		int noOfButtons = 0;
		noOfButtons += showCancel ? 1 : 0;
		noOfButtons += showBack ? 1 : 0;
		noOfButtons += showForward ? 1 : 0;

		int buttonWidth = (getWidth() - ((noOfButtons - 1) * SPACING)) / noOfButtons;
		int buttonHeight = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTONHEIGHT, getResources().getDisplayMetrics());
		ImageAdapter adapter = new ImageAdapter(super.getContext(), controller.getProject(), buttonWidth, buttonHeight);
		adapter.buttonsToDisplay(showBack, showCancel);
		setNumColumns(noOfButtons);
		setPadding(0, 0, 0, SPACING);
		setAdapter(adapter);

		setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View v, int position, long id)
			{
				if(showBack && showCancel)
				{
					if(position == 0)
						controller.goBack();
					if(position == 1)
						controller.restartForm();
					return;
				}
				if(showBack)
					controller.goBack();
				if(showCancel)
					controller.restartForm();
			}
		});
	}
}
