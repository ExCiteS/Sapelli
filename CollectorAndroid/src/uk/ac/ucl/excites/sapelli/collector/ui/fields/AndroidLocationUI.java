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

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.OnPageView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.SpinnerItem;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.util.Timeoutable;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;

/**
 * @author Julia, mstevens, benelliott
 *
 */
public class AndroidLocationUI extends LocationUI<View, CollectorView> {

	private CollectorController controller;
	private LocationOnPageView pageView;
	private RelativeLayout waitView;
	private Timer timeoutCounter = null;
	
	static public final float PADDING = 20.0f;

	public AndroidLocationUI(LocationField field, CollectorController controller, CollectorView collectorUI) {
		super(field, controller, collectorUI);
		this.controller = controller;
	}

	@Override
	protected void cancel() {
		if (timeoutCounter != null)
			timeoutCounter.cancel();
		// else: do nothing
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord) {
		// TODO editable
		if (onPage) {
			if (pageView == null) 
				pageView = new LocationOnPageView(collectorUI.getContext(), controller, this);
			
			pageView.setEnabled(enabled);
			
			return pageView;
		} else {
			// TODO show coordinates/accuracy to literate users (this will need a new XML attribute)
			if (waitView == null) {
				Context context = collectorUI.getContext();
				waitView = new RelativeLayout(context);
				RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				params.addRule(RelativeLayout.CENTER_IN_PARENT);
				ImageView gpsIcon = new ImageView(context);
				gpsIcon.setImageDrawable(context.getResources().getDrawable(R.drawable.gps_location));
				gpsIcon.setScaleType(ScaleType.CENTER_INSIDE);
				int padding = ScreenMetrics.ConvertDipToPx(context, PADDING);
				gpsIcon.setPadding(padding, padding, padding, padding);
				waitView.addView(gpsIcon);
				waitView.addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge), params);
			}

			// Cancel previous counter:
			cancel();

			// Start timeout counter
			timeoutCounter = new Timer();
			timeoutCounter.schedule(new TimerTask() {
				@Override
				public void run() { // time's up!
					collectorUI.getActivity().runOnUiThread(new Runnable() {
						@Override
						public void run() {
							timeout();
						}
					});
				}
			}, ((Timeoutable) field).getTimeoutS() * 1000 /* ms */);

			return waitView;
		}
	}
	
	public void showLocationStoredOnPage() {
		if (pageView == null) // should be impossible
			return;
		pageView.locationStored();
	}

	private class LocationOnPageView extends OnPageView {
		
		private Context context;

		public LocationOnPageView(Context context, CollectorController controller, FieldUI<LocationField, View, CollectorView> fieldUi)
		{
			super(context, controller, fieldUi);
			this.context = context;
			// get location image (TODO make customisable?):
			Item image = new ResourceImageItem(context.getResources(), R.drawable.gps_location_marker);
			if(field.getColumn().isValueSet(controller.getCurrentRecord()))
			{ // already have a valid location for this record, so show this on the page
				locationStored();
				return;
			}
			// else... create an item (with spinner depending on StartWith)
			if(field.getStartWith() == LocationField.StartWith.FIELD)
				this.setContentView(image.getView(context));
			else
			{ // FORM or PAGE: add a spinner on top of the image (remove it when a location is found)
				this.setContentView(new LayeredItem().addLayer(image).addLayer(new SpinnerItem()).getView(context));
			}
		}
		
		/**
		 * Remove the current page representation (including spinner, if present) and replace with one that is overlaid by a tick to represent that a valid store has been made
		 */
		public void locationStored()
		{
			// TODO disable field?
			Item tick = new ResourceImageItem(context.getResources(), R.drawable.button_tick_svg);
			Item image = new ResourceImageItem(context.getResources(), R.drawable.gps_location_marker); // TODO can't simply reuse this item as its view is already associated with the old layered item - remove items?
			tick.setBackgroundColor(Color.TRANSPARENT);
			this.setContentView(new LayeredItem().addLayer(image).addLayer(tick).getView(context));
		}
	}
}
