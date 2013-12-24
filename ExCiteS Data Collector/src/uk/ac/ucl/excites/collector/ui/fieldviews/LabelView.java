package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.LabelField;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.LinearLayout;
import android.widget.TextView;


/**
 * @author mstevens
 *
 */
@SuppressLint("ViewConstructor")
public class LabelView extends LinearLayout implements FieldUI
{
	
	private LabelField field;

	public LabelView(Context context, LabelField field)
	{
		super(context);
		this.field = field;
		
		TextView label = new TextView(context);
		label.setText(field.getText());
		addView(label);
	}

	@Override
	public Field getField()
	{
		return field;
	}

	@Override
	public void update()
	{
		// does nothing
	}

	@Override
	public void cancel()
	{
		// does nothing		
	}

}
