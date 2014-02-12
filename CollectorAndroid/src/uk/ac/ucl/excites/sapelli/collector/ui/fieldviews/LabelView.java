package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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
	public LabelField getField()
	{
		return field;
	}

	@Override
	public void cancel()
	{
		// does nothing		
	}

	@Override
	public void update(Record record)
	{
		// does nothing
	}

	@Override
	public boolean isValid(Record record)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeValue(Record record)
	{
		// TODO Auto-generated method stub
		
	}

}
