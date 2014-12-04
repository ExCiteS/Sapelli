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
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.OnPageView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidOrientationUI extends OrientationUI<View, CollectorView>
{

	private CollectorController controller;
	private OrientationOnPageView pageView;
	private RelativeLayout waitView;

	static public final float PADDING = 40.0f;

	public AndroidOrientationUI(OrientationField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		this.controller = controller;
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		// TODO take "enabled" into account!
		if(onPage)
		{
			if(pageView == null)
				pageView = new OrientationOnPageView(collectorUI.getContext(), controller, this);

			pageView.setEnabled(enabled);
			return pageView;
		}
		else
		{
			if(waitView == null)
			{
				Context context = collectorUI.getContext();
				waitView = new RelativeLayout(context);
				RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				params.addRule(RelativeLayout.CENTER_IN_PARENT);
				ImageView gpsIcon = new ImageView(context);
				gpsIcon.setImageDrawable(context.getResources().getDrawable(R.drawable.compass));
				gpsIcon.setScaleType(ScaleType.CENTER_INSIDE);
				int padding = ScreenMetrics.ConvertDipToPx(context, PADDING);
				gpsIcon.setPadding(padding, padding, padding, padding);
				waitView.addView(gpsIcon);
				waitView.addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge), params);

			}
		}
		return waitView;

	}
	
	private class OrientationOnPageView extends OnPageView
	{

		public OrientationOnPageView(Context context, CollectorController controller, FieldUI<OrientationField, View, CollectorView> fieldUi)
		{
			super(context, controller, fieldUi);
			// get compass image (TODO make customisable?):
			View content = ((new ResourceImageItem(context.getResources(), R.drawable.compass)).getView(context));
			this.setContentView(content);
		}		
	}
}
