/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.OnPageView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;

/**
 * @author Julia, mstevens, benelliott
 *
 */
public class AndroidOrientationUI extends OrientationUI<View, CollectorView>
{

	private OrientationOnPageView pageView;
	private RelativeLayout waitView;

	static public final float PADDING = 40.0f;

	public AndroidOrientationUI(OrientationField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		// TODO take "enabled" into account!
		if(onPage)
		{
			if(pageView == null)
				pageView = new OrientationOnPageView(collectorUI.getContext(), controller, collectorUI, this);

			pageView.setEnabled(enabled);
			
			if(field.getColumn().isValueSet(controller.getCurrentRecord()))
				// already have a valid location for this record, so show this on the page -- check this every time the page is shown
					pageView.orientationStored();
			
			return pageView;
		}
		else
		{
			if(waitView == null)
			{
				Context context = collectorUI.getContext();
				waitView = new RelativeLayout(context);
				View compassIcon = new ResourceImageItem(context.getResources(), R.drawable.compass_svg).setBackgroundColor(Color.BLACK).getView(context);
				int padding = ScreenMetrics.ConvertDipToPx(context, PADDING);
				compassIcon.setPadding(padding, padding, padding, padding);
				waitView.addView(compassIcon, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
				RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				params.addRule(RelativeLayout.CENTER_IN_PARENT);
				waitView.addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge), params);

			}
		}
		return waitView;

	}
	
	private class OrientationOnPageView extends OnPageView
	{

		private Context context;
		
		public OrientationOnPageView(Context context, Controller<CollectorView> controller, CollectorView collectorView, FieldUI<OrientationField, View, CollectorView> fieldUI)
		{
			super(context, controller, collectorView, fieldUI);
			this.context = context;
			// get compass image (TODO make customisable?):
			this.setContentView((new ResourceImageItem(context.getResources(), R.drawable.compass_svg)).getView(context));
		}
		
		private void orientationStored()
		{
			Item tick = new ResourceImageItem(context.getResources(), R.drawable.button_tick_svg);
			Item image = new ResourceImageItem(context.getResources(), R.drawable.compass_svg);
			tick.setBackgroundColor(Color.TRANSPARENT);
			this.setContentView(new LayeredItem().addLayer(image).addLayer(tick).getView(context)); // TODO creating new item so views are fresh
		}
	}
}
