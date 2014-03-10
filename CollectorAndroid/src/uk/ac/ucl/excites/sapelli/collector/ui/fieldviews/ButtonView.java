/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import uk.ac.ucl.excites.sapelli.collector.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.annotation.SuppressLint;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

/**
 * A FieldUI/View class for ButtonFields
 * 
 * @author mstevens
 */
@SuppressLint("ViewConstructor")
public class ButtonView extends Button implements FieldUI, OnClickListener
{

	private ButtonField field;
	
	/**
	 * @param context
	 */
	public ButtonView(Context context, ButtonField field)
	{
		super(context);
		this.field = field;
		this.setText(field.getLabel());
		this.setOnClickListener(this);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#getField()
	 */
	@Override
	public ButtonField getField()
	{
		return field;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#update()
	 */
	@Override
	public void update(Record record)
	{
		// does nothing
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#cancel()
	 */
	@Override
	public void cancel()
	{
		// does nothing
	}

	@Override
	public void onClick(View v)
	{
		// TODO carry out the button jump
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
