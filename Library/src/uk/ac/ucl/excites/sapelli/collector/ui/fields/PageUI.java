/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.NonSelfLeavingFieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * @author mstevens
 *
 */
public abstract class PageUI<V> extends NonSelfLeavingFieldUI<Page, V>
{

	protected List<FieldUI<?, V>> fieldUIs;
	
	public PageUI(Page page, Controller controller, CollectorUI<V> collectorUI)
	{
		super(page, controller, collectorUI);
		fieldUIs = new ArrayList<FieldUI<?, V>>();
		
		for(Field f : page.getFields())
			CollectionUtils.addIgnoreNull(fieldUIs, f.createUI(collectorUI));
	}
	
	@Override
	public void cancel()
	{
		for(FieldUI<?, V> fUI : fieldUIs)
			fUI.cancel();
	}
	
	@Override
	public boolean leave(CollectorRecord record, boolean noValidation)
	{
		if(noValidation || isValid(record))
		{
			for(FieldUI<?, V> fUI : fieldUIs)
				if(!fUI.getField().isNoColumn())
					fUI.leave(record, true); // skip validation (otherwise we'd repeat it), this means that NonSelfLeavingFieldUIs will only store their value
			return true;
		}
		return false;
	}	

	@Override
	public boolean isValid(CollectorRecord record)
	{
		for(FieldUI<?, V> fUI : fieldUIs)
			if(!fUI.isValid(record))
				return false;
		return true;
	}

	@Override
	protected void storeValue(CollectorRecord record)
	{
		// does nothing (Pages have no column of their own)
	}

}
