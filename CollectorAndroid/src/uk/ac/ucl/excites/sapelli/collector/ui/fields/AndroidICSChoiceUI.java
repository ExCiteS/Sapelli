/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Iterator;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.annotation.TargetApi;
import android.content.Context;
import android.graphics.Color;
import android.os.Build;
import android.view.View;
import android.widget.GridLayout;

/**
 * ChoiceUi implementation for Android Ice Cream Sandwich and above,
 * the difference with the normal AndroidChoiceUI is rowSpan/colSpan support.
 * 
 * @author mstevens
 */
@TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH)
public class AndroidICSChoiceUI extends AndroidChoiceUI
{

	public AndroidICSChoiceUI(ChoiceField choice, CollectorController controller, CollectorView collectorView)
	{
		super(choice, controller, collectorView);
	}
	
	@Override
	public ChoiceView getChoiceView()
	{
		return new ICSChoiceView(collectorUI.getContext());
	}
	
	private class ICSChoiceView extends GridLayout implements ChoiceView
	{

		public ICSChoiceView(Context context)
		{
			super(context);
			
			// UI set-up:
			setOrientation(HORIZONTAL);
			setColumnCount(field.getRows());
			setRowCount(field.getRows());
			
			setBackgroundColor(Color.BLACK);
			int spacingPx = collectorUI.getSpacingPx();
			
			//TODO spacing
			//setHorizontalSpacing(spacingPx);
			//setVerticalSpacing(spacingPx);
			
			int itemPaddingPx = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
			
			Iterator<ChoiceField> children = field.getChildren().iterator();
			int r = 0, c = 0;
			while(children.hasNext())
			{
				final ChoiceField child = children.next();
				View view = createItem(child, itemPaddingPx, !field.isEnabled()).getView(getContext());
				view.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						onChildClick(child, v);
					}
				});
				LayoutParams params = new LayoutParams(	GridLayout.spec(r, child.getRowSpan(), FILL),
														GridLayout.spec(c, child.getColSpan(), FILL));
				// This can't be right??:
				r = r + child.getRowSpan() % field.getRows();
				c = c + child.getColSpan() % field.getCols();
				addView(view, params);
				
			}
			
			
		}

		@Override
		public void update()
		{
			// TODO Auto-generated method stub
			
		}
		
	}

}
