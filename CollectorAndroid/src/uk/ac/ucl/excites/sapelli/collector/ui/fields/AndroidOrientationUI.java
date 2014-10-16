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
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationListener;
import uk.ac.ucl.excites.sapelli.collector.geo.OrientationSensor;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.RectF;
import android.util.Log;
import android.view.Gravity;
import android.view.SurfaceHolder;
import android.view.SurfaceHolder.Callback;
import android.view.SurfaceView;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidOrientationUI extends OrientationUI<View, CollectorView> implements OrientationListener {

	private Button pageView;
	private RelativeLayout waitView;
	private OrientationSensor orientationSensor;
	private OrientationDisplaySurfaceView odsv;
	private Orientation orientation;

	static public final float PADDING = 40.0f;

	public AndroidOrientationUI(OrientationField field, Controller controller, CollectorView collectorUI) {
		super(field, controller, collectorUI);
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord) {
		// TODO take "enabled" into account!
		Context context = collectorUI.getContext();
		
		if (onPage) {
			if (pageView == null) {
				pageView = new Button(context);
				pageView.setText(field.getCaption());
				// TODO some kind of icon/image would be nice (an arrow?)
				pageView.setOnClickListener(new OnClickListener() {
					@Override
					public void onClick(View v) {
						controller.goTo(new FieldWithArguments(field), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the field itself
					}
				});
			}
			return pageView;
		} else {
			if (waitView == null) {
				waitView = new RelativeLayout(context);
				ImageView gpsIcon = new ImageView(context);
				gpsIcon.setImageDrawable(context.getResources().getDrawable(R.drawable.compass));
				gpsIcon.setScaleType(ScaleType.CENTER_INSIDE);
				int padding = ScreenMetrics.ConvertDipToPx(context, PADDING);
				gpsIcon.setPadding(padding, padding, padding, padding);
				waitView.addView(gpsIcon);
				odsv = new OrientationDisplaySurfaceView(context);
				RelativeLayout.LayoutParams compassParams = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				compassParams.addRule(RelativeLayout.CENTER_IN_PARENT);
				waitView.addView(odsv, compassParams);
				RelativeLayout.LayoutParams confirmParams = new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
				waitView.addView(new OrientationConfirmView(context),confirmParams);
			}
		}
		
		if (orientationSensor == null)
			orientationSensor = new OrientationSensor(context);
		orientationSensor.start(this); // start listening for orientation updates
		
		return waitView;
	}
	
	private class OrientationDisplaySurfaceView extends SurfaceView {

		private static final int COLOR_BACKGROUND = Color.BLACK;
		private static final int COLOR_COMPASS = Color.DKGRAY;
		private static final int COLOR_INNER_DIAL = Color.RED;
		private static final int COMPASS_RADIUS = 400;
		private static final int INNER_DIAL_RADIUS = 350;
		private static final int MARKER_WIDTH = 30;
		private Paint paint;
		private RectF compassTemplateBounds;
		private RectF innerDialBounds;
		private float compassCentreX;
		private float compassCentreY;

		public OrientationDisplaySurfaceView(Context context) {
			super(context);
			paint = new Paint();			
			getHolder().addCallback(new Callback(){

				@Override
				public void surfaceCreated(SurfaceHolder holder) {
				}

				@SuppressLint("WrongCall")
                @Override
				public void surfaceChanged(SurfaceHolder holder, int format,
						int width, int height) {
					
					compassCentreX = width / 2;
					compassCentreY = height / 2;
					compassTemplateBounds = new RectF(compassCentreX - COMPASS_RADIUS, compassCentreY - COMPASS_RADIUS, compassCentreX + COMPASS_RADIUS, compassCentreY + COMPASS_RADIUS);
					innerDialBounds = new RectF(compassCentreX - INNER_DIAL_RADIUS, compassCentreY - INNER_DIAL_RADIUS, compassCentreX + INNER_DIAL_RADIUS, compassCentreY + INNER_DIAL_RADIUS);
					
					Canvas c = holder.lockCanvas(null);
					onDraw(c);
					holder.unlockCanvasAndPost(c);
				}

				@Override
				public void surfaceDestroyed(SurfaceHolder holder) {
				}});
		}

		/**
		 * Draw the compass to screen.
		 */
		@Override
		protected void onDraw(Canvas canvas) {
			if (orientation != null) {
				float azimuth = orientation.getAzimuth();
				// wipe everything off: 
				canvas.drawColor(COLOR_BACKGROUND);
				// draw compass template:
				paint.setColor(COLOR_COMPASS);
				canvas.drawOval(compassTemplateBounds, paint);
				// draw new orientation:
				paint.setColor(COLOR_INNER_DIAL);
				Log.d("Compass", "Azimuth: "+azimuth);
				// rotate azimuth by 270 deg (since 0 deg is at x=0) then draw:
				canvas.drawArc(compassTemplateBounds, azimuth + 270f - MARKER_WIDTH / 2, MARKER_WIDTH, true, paint);
				paint.setColor(COLOR_BACKGROUND);
				canvas.drawOval(innerDialBounds, paint);
			}
		}
		
		@SuppressLint("WrongCall")
        protected void drawOrientation(Orientation orientation) {
			Canvas c = null;
			try {				
				c = getHolder().lockCanvas();
				synchronized (getHolder()) {
					if (c != null)
						onDraw(c);
				}
			} finally {
				if (c != null) {
					getHolder().unlockCanvasAndPost(c);
				}
			}
		}
	}

	@Override
    public void onOrientationChanged(Orientation orientation) {
		this.orientation = orientation;
	    odsv.drawOrientation(orientation);
    }
	
	private class OrientationConfirmView extends PickerView
	{
		private int buttonPadding;
		private int buttonBackColor;

		public OrientationConfirmView(Context context)
		{
			super(context);

			this.setOnItemClickListener(new OnItemClickListener() {
				@Override
				public void onItemClick(AdapterView<?> parent, View view,
						int position, long id) {
						Runnable buttonAction = new Runnable() {
							@Override
							public void run() {
								field.storeOrientation(controller.getCurrentRecord(), orientation);
								orientationSensor.stop(); // stop listening for updates
								controller.goForward(false);
							}
						};
						// Execute the "press" animation if allowed, then perform the action: 
						if(controller.getCurrentForm().isClickAnimation())
							(new ClickAnimator(buttonAction, view, collectorUI)).execute(); //execute animation and the action afterwards
						else
							buttonAction.run(); //perform task now (animation is disabled)
				}	
			});

			// Layout:
			setBackgroundColor(Color.TRANSPARENT);
			setGravity(Gravity.CENTER);
			setPadding(0, collectorUI.getSpacingPx(), 0, 0);

			// Columns
			setNumColumns(1);
			
			// Button size, padding & background colour:
			this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP * 3);
			this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);

			Item approveButton = null;
//			File approveImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getApproveButtonImageRelativePath());
//			if(FileHelpers.isReadableFile(approveImgFile))
//				approveButton = new FileImageItem(approveImgFile);
//			else
				approveButton = new ResourceImageItem(getContext().getResources(), R.drawable.button_tick_svg);
			approveButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
			
			approveButton.setPaddingPx(buttonPadding);
			approveButton.setBackgroundColor(buttonBackColor);
			getAdapter().addItem(approveButton);

			// And finally:
			setAdapter(getAdapter()); // this is supposedly needed on Android v2.3.x (TODO test it)
		}
	}

}
