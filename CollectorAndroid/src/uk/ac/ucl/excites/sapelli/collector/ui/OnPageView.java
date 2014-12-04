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

package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * TODO
 * 
 * @author benelliott, mstevens
 *
 */
public class OnPageView extends LinearLayout implements OnClickListener, OnFocusChangeListener
{
	static public final float CONTENT_DEFAULT_WIDTH_DIP = 60.0f;
	static public final float CONTENT_DEFAULT_HEIGHT_DIP = 60.0f;
	static public final float CONTENT_DEFAULT_MARGIN_DIP = 1.0f; // same margin all round


	private CollectorController controller;
	private FieldUI<?, View, CollectorView> fieldUi;
	private TextView label;
	private View contentView;

	private int contentWidthPx;
	private int contentHeightPx;
	private int contentPaddingPx;
	private int contentMarginPx;

	private LinearLayout.LayoutParams contentParams;

	public OnPageView(Context context, CollectorController controller, FieldUI<?, View, CollectorView> fieldUi)
	{
		super(context);
		this.controller = controller;
		this.fieldUi = fieldUi;

		contentWidthPx = ScreenMetrics.ConvertDipToPx(context, CONTENT_DEFAULT_WIDTH_DIP);
		contentHeightPx = ScreenMetrics.ConvertDipToPx(context, CONTENT_DEFAULT_HEIGHT_DIP);
		contentPaddingPx = ScreenMetrics.ConvertDipToPx(context, CollectorView.PADDING_DIP);
		contentMarginPx = ScreenMetrics.ConvertDipToPx(context, CONTENT_DEFAULT_MARGIN_DIP);

		contentParams = new LayoutParams(contentWidthPx, contentHeightPx);
		contentParams.setMargins(contentMarginPx, contentMarginPx, contentMarginPx, contentMarginPx);

		this.setOrientation(LinearLayout.VERTICAL);
		// TODO this layout params?
		label = new TextView(context);
		//ensure that the label text is not truncated, by setting width to WRAP_CONTENT:
		label.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
		label.setText(fieldUi.getField().getCaption());
		this.addView(label);
	}

	public void setContentView(View contentView)
	{
		if(this.contentView != null)
			this.removeView(this.contentView);

		this.contentView = contentView;

		contentView.setLayoutParams(contentParams);
		contentView.setPadding(contentPaddingPx, contentPaddingPx, contentPaddingPx, contentPaddingPx);

		this.addView(contentView);
	}

	@Override
	public void setEnabled(boolean enabled)
	{
		super.setEnabled(enabled);
		if(contentView != null)
		{
			contentView.setEnabled(enabled);
			contentView.setOnClickListener(enabled ? this : null);
			// Make other fields lose focus and simulate clicking with onFocusChange:
			contentView.setFocusable(enabled);
			contentView.setFocusableInTouchMode(enabled);
			contentView.setOnFocusChangeListener(enabled ? this : null);
		}
	}

	@Override
	public void onFocusChange(View v, boolean hasFocus)
	{
		if(!fieldUi.isFieldShown() || !this.isEnabled() || !v.isEnabled())
			return;
	
		Log.d("OnPageView","Focus changed. Has focus: "+hasFocus);
		if(hasFocus)
		{
			// Lose focus again:
			v.clearFocus();

			// Simulate click:
			onClick(v);
		}
		else
			fieldUi.isValidInformPage(controller.getCurrentRecord()); // will call isValid() but via the containing page such that the red box can (dis)appear, if the field is not on a page isValid() is called directly

	}

	@Override
	public void onClick(View v)
	{
		// Do nothing if not shown or enabled:
		if(!fieldUi.isFieldShown() || !isEnabled() || !v.isEnabled())
			return;

		// The user will make a choice now, so don't annoy him/her with the red box:
		//fieldUi.clearPageInvalidMark();

		// Task to perform after animation has finished:
		Runnable action = new Runnable()
		{
			public void run()
			{
				controller.goTo(new FieldWithArguments(fieldUi.getField()), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the
																													// field itself
			}
		};

		// Perform the click
		controller.clickView(v, action);
	}

	public void setContentWidthPx(int contentWidthPx)
	{
		this.contentWidthPx = contentWidthPx;
		contentParams.width = contentWidthPx;

		setContentView(contentView);
	}

	public int getContentWidthPx()
	{
		return contentWidthPx;
	}

	public void setContentHeightPx(int contentHeightPx)
	{
		this.contentHeightPx = contentHeightPx;
		contentParams.height = contentHeightPx;

		setContentView(contentView);
	}

	public int getContentHeightPx()
	{
		return contentHeightPx;
	}

	public void setContentMarginPx(int contentMarginPx)
	{
		this.contentMarginPx = contentMarginPx;
		contentParams.setMargins(contentMarginPx, contentMarginPx, contentMarginPx, contentMarginPx);

		setContentView(contentView);
	}

	public int getContentMarginPx()
	{
		return contentMarginPx;
	}

	public void setContentPaddingPx(int contentPaddingPx)
	{
		this.contentPaddingPx = contentPaddingPx;

		setContentView(contentView);
	}

	public int getContentPaddingPx()
	{
		return contentPaddingPx;
	}
}
