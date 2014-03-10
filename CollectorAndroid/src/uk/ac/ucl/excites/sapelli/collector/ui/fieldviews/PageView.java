package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import uk.ac.ucl.excites.sapelli.collector.project.model.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.annotation.SuppressLint;
import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ScrollView;

/**
 * @author mstevens, Michalis Vitos
 * 
 */
@SuppressLint("ViewConstructor")
public class PageView extends ScrollView implements FieldUI
{

	static public final int CHILD_PADDING = 10;
	
	private Page page;
	private LinearLayout container;
	
	/**
	 * @param context
	 * @param attrs
	 */
	public PageView(Context context, CollectorView collectorView, Page page)
	{
		super(context);
		this.page = page;
		
		// Container (LinearLayout with vertical orientation):
		this.container = new LinearLayout(context);
		container.setOrientation(LinearLayout.VERTICAL);
		this.addView(container);
		
		int paddingPx = ScreenMetrics.ConvertDipToPx(context, CHILD_PADDING);
		for(Field f : page.getFields())
		{
			//TODO showOnCreate/showOnEdit
			View child = (View) f.createUI(collectorView);
			child.setPadding(0, 0, 0, paddingPx);
			container.addView(child);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#getField()
	 */
	@Override
	public Page getField()
	{
		return page;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#update(uk.ac.ucl.excites.storage.model.Record)
	 */
	@Override
	public void update(Record record)
	{
		int childcount = container.getChildCount();
		for(int i = 0; i < childcount; i++)
			((FieldUI) container.getChildAt(i)).update(record);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#cancel()
	 */
	@Override
	public void cancel()
	{
		int childcount = container.getChildCount();
		for(int i = 0; i < childcount; i++)
			((FieldUI) container.getChildAt(i)).cancel();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#isValid(uk.ac.ucl.excites.storage.model.Record)
	 */
	@Override
	public boolean isValid(Record record)
	{
		int childcount = container.getChildCount();
		for(int i = 0; i < childcount; i++)
			if(!((FieldUI) container.getChildAt(i)).isValid(record))
				return false;
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#storeValue(uk.ac.ucl.excites.storage.model.Record)
	 */
	@Override
	public void storeValue(Record record)
	{
		int childcount = container.getChildCount();
		for(int i = 0; i < childcount; i++)
			((FieldUI) container.getChildAt(i)).storeValue(record);
	}

}
