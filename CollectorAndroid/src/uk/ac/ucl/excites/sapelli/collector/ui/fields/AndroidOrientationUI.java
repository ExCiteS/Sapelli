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
import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
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

	private Button pageView;
	private RelativeLayout waitView;

	static public final float PADDING = 40.0f;

	public AndroidOrientationUI(OrientationField field, Controller controller, CollectorView collectorUI)
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
			{
				pageView = new Button(collectorUI.getContext());
				pageView.setText(field.getCaption());
				// TODO some kind of icon/image would be nice (an arrow?)
				pageView.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						controller.goTo(new FieldWithArguments(field), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the field
																											// itself
					}
				});
			}
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
}
