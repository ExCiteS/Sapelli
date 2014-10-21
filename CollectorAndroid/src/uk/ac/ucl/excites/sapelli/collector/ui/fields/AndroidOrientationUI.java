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

import java.io.File;

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
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Path.FillType;
import android.graphics.PointF;
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
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout.LayoutParams;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidOrientationUI extends OrientationUI<View, CollectorView> implements OrientationListener {

	private Button pageView;
	private LinearLayout waitView;
	private OrientationSensor orientationSensor;
	private CompassView compass;
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
				// show a spinner initially, which is replaced with a compass when an orientation is received for the first time
				waitView = new LinearLayout(context);
				waitView.setOrientation(LinearLayout.VERTICAL);
				ImageView gpsIcon = new ImageView(context);
				gpsIcon.setImageDrawable(context.getResources().getDrawable(R.drawable.compass));
				gpsIcon.setScaleType(ScaleType.CENTER_INSIDE);
				int padding = ScreenMetrics.ConvertDipToPx(context, PADDING);
				gpsIcon.setPadding(padding, padding, padding, padding);
				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
				params.gravity = Gravity.CENTER_HORIZONTAL;
				//waitView.addView(gpsIcon, params);
				waitView.addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge), params);
			}
		}
		
		if (orientationSensor == null)
			orientationSensor = new OrientationSensor(context);
		orientationSensor.start(this); // start listening for orientation updates after views are created

		return waitView;
	}

	@Override
	public void onOrientationChanged(Orientation orientation) {
		this.orientation = orientation;
		if (compass == null) {
			Context context = waitView.getContext();
			// currently showing spinner, must clear and replace with compass and confirmation button
			waitView.removeAllViews();
			// set the container's weight sum so the compass can expand to fit the available area:
			waitView.setWeightSum(1.0f);
			// create compass
			compass = new CompassView(context);
			LinearLayout.LayoutParams compassParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			compassParams.gravity = Gravity.CENTER_HORIZONTAL;
			// compass should expand to fit any available space:
			compassParams.weight = 1.0f;
			waitView.addView(compass, compassParams);
			LinearLayout.LayoutParams confirmParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
			confirmParams.gravity = Gravity.BOTTOM;
			waitView.addView(new OrientationConfirmView(context),confirmParams);
		}
		compass.drawOrientationToScreen();
	}
	
	@Override
	public void cancel() {
		compass = null;
		if (orientationSensor != null) {
			orientationSensor.stop();
			orientationSensor = null;
		}
		waitView = null;
	}
	
	private class CompassView extends SurfaceView {

		private static final int COLOR_BACKGROUND = Color.BLACK;
		private static final int COLOR_OUTER = Color.GRAY;
		private static final int COLOR_INNER = Color.DKGRAY;
		private static final int COLOR_NORTH_POINTER = Color.RED;
		private static final int COLOR_SOUTH_POINTER = Color.WHITE;
		private static final float INNER_CIRCLE_PERCENTAGE = 0.92f; // inner compass circle radius as percentage of outer radius
		private static final float POINTER_BASE_WIDTH = 80;
		private static final float COMPASS_ARROW_SPACING = 20; //vertical gap between arrow and compass
		private static final float ARROW_WIDTH_PERCENTAGE = 0.4f; // arrow width as percentage of View's width 
		private static final float ARROW_HEIGHT_PERCENTAGE_WIDTH = 0.3f; // arrow height as percentage of arrow width
		private static final boolean ANTI_ALIAS = true;
		private Bitmap arrowBitmap; // TODO expose arrow in XML so that custom images can be used?
		private Bitmap compassBitmap;
		private Matrix compassRotationMatrix;

		public CompassView(Context context) {
			super(context);
			compassRotationMatrix = new Matrix();
			getHolder().addCallback(new Callback(){

				@Override
				public void surfaceCreated(SurfaceHolder holder) {
				}

				@SuppressLint("WrongCall")
				@Override
				public void surfaceChanged(SurfaceHolder holder, int format,
						int width, int height) {

					// may need to resize compass and arrow, so regenerate bitmaps
					arrowBitmap = createArrowBitmap((int) (width * ARROW_WIDTH_PERCENTAGE));

					// want width of compass equal to height so that compass is not stretched
					int size = (width < height) ? width : height;
					compassBitmap = createCompassBitmap(size);

					Canvas c = holder.lockCanvas(null);
					onDraw(c);
					holder.unlockCanvasAndPost(c);
				}

				@Override
				public void surfaceDestroyed(SurfaceHolder holder) {
				}});
		}

		/**
		 *  Rotate the compass according to the azimuth and draw it to the canvas.
		 */
		@Override
		protected void onDraw(Canvas canvas) {
			if (orientation != null) {
				// rotate the compass by the azimuth, about its centre:
				compassRotationMatrix.reset();
				compassRotationMatrix.postTranslate(0, COMPASS_ARROW_SPACING + arrowBitmap.getHeight());
				compassRotationMatrix.postRotate(orientation.getAzimuth(), compassBitmap.getWidth() / 2, arrowBitmap.getHeight() + COMPASS_ARROW_SPACING + compassBitmap.getHeight() / 2);
				canvas.drawBitmap(compassBitmap, compassRotationMatrix, null);

				// draw top arrow last in case corner of compass bitmap overlaps it:
				canvas.drawBitmap(arrowBitmap, (getWidth() - arrowBitmap.getWidth()) / 2, 0, null);
			}
		}

		@SuppressLint("WrongCall")
		protected void drawOrientationToScreen() {
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

		private Bitmap createCompassBitmap(int size) {
			Bitmap bitmap = Bitmap.createBitmap(size, size, Bitmap.Config.ARGB_8888);

			float bitmapCentreX = bitmap.getWidth() / 2;
			float bitmapCentreY = bitmap.getHeight() / 2;

			Canvas bitmapCanvas = new Canvas(bitmap);
			Paint paint = new Paint();
			paint.setAntiAlias(ANTI_ALIAS);

			// draw background:
			bitmapCanvas.drawColor(COLOR_BACKGROUND);

			// draw outer circle:
			paint.setColor(COLOR_OUTER);
			RectF outerCircleBounds = new RectF(0, 0, size, size);
			bitmapCanvas.drawOval(outerCircleBounds, paint);

			// draw inner circle:
			paint.setColor(COLOR_INNER);
			RectF innerCircleBounds = new RectF(outerCircleBounds.right * (1 - INNER_CIRCLE_PERCENTAGE), outerCircleBounds.bottom * (1 - INNER_CIRCLE_PERCENTAGE), outerCircleBounds.right * INNER_CIRCLE_PERCENTAGE, outerCircleBounds.bottom * INNER_CIRCLE_PERCENTAGE);
			bitmapCanvas.drawOval(innerCircleBounds, paint);

			// draw north-pointing triangle:
			paint.setColor(COLOR_NORTH_POINTER);
			PointF north = new PointF(bitmapCentreX, innerCircleBounds.top); //top vertex
			PointF left = new PointF(bitmapCentreX - POINTER_BASE_WIDTH/2, bitmapCentreY); // bottom-left vertex
			PointF right = new PointF(bitmapCentreX + POINTER_BASE_WIDTH/2, bitmapCentreY); //bottom-right vertex
			Path northPointer = new Path();
			northPointer.setFillType(FillType.EVEN_ODD);
			northPointer.moveTo(right.x, right.y);
			northPointer.lineTo(north.x, north.y);
			northPointer.lineTo(left.x, left.y);
			northPointer.lineTo(right.x, right.y);
			northPointer.close();
			bitmapCanvas.drawPath(northPointer, paint);

			// draw south-pointing triangle:
			paint.setColor(COLOR_SOUTH_POINTER); 
			PointF south = new PointF(bitmapCentreX, innerCircleBounds.bottom); //bottom vertex
			Path southPointer = new Path();
			southPointer.setFillType(FillType.EVEN_ODD);
			southPointer.moveTo(right.x, right.y); //use existing left/right vertices
			southPointer.lineTo(south.x, south.y);
			southPointer.lineTo(left.x, left.y);
			southPointer.lineTo(right.x, right.y);
			southPointer.close();
			bitmapCanvas.drawPath(southPointer, paint);

			return bitmap;
		}

		private Bitmap createArrowBitmap(int arrowWidth) {
			Log.d("Arrow","width: "+arrowWidth);
			Bitmap bitmap = Bitmap.createBitmap(arrowWidth,(int) (arrowWidth * ARROW_HEIGHT_PERCENTAGE_WIDTH), Bitmap.Config.ARGB_8888);
			Log.d("Arrow","height: "+bitmap.getHeight());
			Canvas bitmapCanvas = new Canvas(bitmap);
			Paint paint = new Paint();
			paint.setAntiAlias(ANTI_ALIAS);

			// draw simple isosceles triangle:
			paint.setColor(COLOR_OUTER);
			PointF top = new PointF(bitmap.getWidth() / 2, 0); //top vertex
			PointF left = new PointF(0, bitmap.getHeight()); // bottom-left vertex
			PointF right = new PointF(bitmap.getWidth(), bitmap.getHeight()); //bottom-right vertex
			Path arrowPath = new Path();
			arrowPath.setFillType(FillType.EVEN_ODD);
			arrowPath.moveTo(right.x, right.y);
			arrowPath.lineTo(top.x, top.y);
			arrowPath.lineTo(left.x, left.y);
			arrowPath.lineTo(right.x, right.y);
			arrowPath.close();
			bitmapCanvas.drawPath(arrowPath, paint);

			return bitmap;
		}
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
			//setPadding(0, collectorUI.getSpacingPx(), 0, 0);

			// Columns
			setNumColumns(1);

			// Button size, padding & background colour:
			this.setItemDimensionsPx(LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(context, AndroidControlsUI.CONTROL_HEIGHT_DIP));
			this.buttonPadding = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP * 3);
			this.buttonBackColor = ColourHelpers.ParseColour(controller.getCurrentForm().getButtonBackgroundColor(), Form.DEFAULT_BUTTON_BACKGROUND_COLOR /*light gray*/);

			Item approveButton = null;
			File approveImgFile = controller.getProject().getImageFile(controller.getFileStorageProvider(),field.getApproveButtonImageRelativePath());
			if(FileHelpers.isReadableFile(approveImgFile))
				approveButton = new FileImageItem(approveImgFile);
			else
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
