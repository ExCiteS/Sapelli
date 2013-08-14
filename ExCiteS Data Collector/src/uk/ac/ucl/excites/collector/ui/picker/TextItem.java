/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.picker;

import uk.ac.ucl.excites.collector.ui.FontFitTextView;
import android.content.Context;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.widget.TextView;

/**
 * @author mstevens
 *
 */
public class TextItem extends Item
{

	private String text;
	
	public TextItem(String text)
	{
		this.text = text;
	}
	
	@Override
	protected View getView(Context context)
	{
		TextView txtView = new FontFitTextView(context);
		txtView.setTextColor(Color.BLACK);
		txtView.setGravity(Gravity.CENTER_VERTICAL | Gravity.CENTER_HORIZONTAL);
		txtView.setText(text);
		return txtView;
	}

}
