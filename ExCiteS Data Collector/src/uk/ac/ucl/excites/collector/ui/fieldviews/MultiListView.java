package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.fields.lists.MultiListField;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.LinearLayout;

@SuppressLint("ViewConstructor")
public class MultiListView extends LinearLayout implements FieldUI
{

	private MultiListField field;
	
	public MultiListView(Context context, MultiListField field)
	{
		super(context);
		this.field = field;
	}

	@Override
	public MultiListField getField()
	{
		return field;
	}

	@Override
	public void update()
	{
		// TODO Auto-generated method stub
	}

	@Override
	public void cancel()
	{
		// TODO Auto-generated method stub
	}

}
