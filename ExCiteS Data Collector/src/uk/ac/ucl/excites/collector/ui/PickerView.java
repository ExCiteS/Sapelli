/**
 * 
 */
package uk.ac.ucl.excites.collector.ui;

import android.content.Context;
import android.graphics.Color;
import android.widget.GridView;

/**
 * @author Julia
 *
 */
public abstract class PickerView extends GridView
{

	static protected final int SPACING = 10;

	protected ImageAdapter imageAdapter;
	
	public PickerView(Context context)
	{
		super(context);
		
		// UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(10);
		setVerticalSpacing(10);
		setAdapter(imageAdapter);
	}

}
