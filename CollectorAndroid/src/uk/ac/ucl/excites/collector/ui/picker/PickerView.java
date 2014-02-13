/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import uk.ac.ucl.excites.collector.R;
import android.content.Context;
import android.widget.GridView;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public abstract class PickerView extends GridView
{

	protected PickerAdapter pickerAdapter;
	
	public PickerView(Context context)
	{
		super(context);
		
		// This is needed to hide the border when an picker item is pressed and to calculate the borders more appropriately
		setSelector(R.drawable.picker_view_selector);
	}

}
