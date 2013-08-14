/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import uk.ac.ucl.excites.collector.R;
import uk.ac.ucl.excites.collector.util.UiUtilites;
import android.content.Context;
import android.graphics.Color;
import android.widget.GridView;

/**
 * @author Julia, Michalis Vitos
 * 
 */
public abstract class PickerView extends GridView
{

	private static final int SPACING = 8; // in dp
	
	protected PickerAdapter pickerAdapter;
	
	public PickerView(Context context)
	{
		super(context);
		
		// UI set-up:
		setBackgroundColor(Color.BLACK);
		setHorizontalSpacing(getSpacingInDp(context));
		setVerticalSpacing(getSpacingInDp(context));
		// This is needed to hide the border when an picker item is pressed and to calculate the borders more appropriately
		setSelector(R.drawable.picker_view_selector);
	}

	/**
	 * @return the spacing
	 */
	public static int getSpacingInPx()
	{
		return SPACING;
	}

	/**
	 * @return the spacing
	 */
	public static int getSpacingInDp(Context context)
	{
		return UiUtilites.dpToPx(context, SPACING);
	}

}
