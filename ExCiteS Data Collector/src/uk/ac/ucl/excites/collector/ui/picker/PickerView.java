/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

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
	
	protected PickerAdapter pickerAdapter;
	
	public PickerView(Context context)
	{
		super(context);
		
		// UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(SPACING);
		setVerticalSpacing(SPACING);
	}

}
