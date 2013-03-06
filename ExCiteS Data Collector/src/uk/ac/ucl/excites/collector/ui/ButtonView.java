/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.ui.images.ImageAdapter;
import uk.ac.ucl.excites.collector.ui.images.ResourceImage;
import android.content.Context;
import android.util.TypedValue;
import android.view.View;
import android.widget.AdapterView;

/**
 * @author Julia, mstevens
 * 
 */
public class ButtonView extends PickerView
{
	public static float BUTTONHEIGHT = 45;

	public ButtonView(Context context)
	{
		super(context);
	}

	public void setButtonView(final ProjectController controller, int viewWidth, final boolean showCancel, final boolean showBack, boolean showForward)
	{
		int noOfButtons = 0;
		noOfButtons += showCancel ? 1 : 0;
		noOfButtons += showBack ? 1 : 0;
		noOfButtons += showForward ? 1 : 0;

		setNumColumns(noOfButtons);
		
		// Adapter & images
		int buttonWidth = (viewWidth - ((noOfButtons - 1) * SPACING)) / noOfButtons;
		int buttonHeight = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, BUTTONHEIGHT, getResources().getDisplayMetrics());
		ImageAdapter adapter = new ImageAdapter(super.getContext(), buttonWidth, buttonHeight);
		if(showBack)
			adapter.addImage(new ResourceImage(R.drawable.back));
		if(showCancel)
			adapter.addImage(new ResourceImage(R.drawable.cancel));
		setAdapter(adapter);
		
		setPadding(0, 0, 0, SPACING);

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
