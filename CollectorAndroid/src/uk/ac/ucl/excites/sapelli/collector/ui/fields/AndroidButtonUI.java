/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

/**
 * Android version of ButtonUI
 * 
 * @author mstevens
 */
public class AndroidButtonUI extends ButtonUI<View, CollectorView> implements OnClickListener
{
	
	private Button button;
	
	public AndroidButtonUI(ButtonField buttonField, CollectorController controller, CollectorView collectorView)
	{
		super(buttonField, controller, collectorView);
	}

	@Override
	public void onClick(View v)
	{
		buttonPressed();
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(button == null)
		{
			button = new Button(collectorUI.getContext());
			button.setText(field.getCaption());
			button.setOnClickListener(this);
		}
		
		// Update:
		button.setEnabled(enabled);
		
		return button;
	}

}
