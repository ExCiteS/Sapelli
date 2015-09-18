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

package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class TransmissionTabFragment extends ProjectManagerTabFragment implements OnClickListener
{

	// Layouts
	private LinearLayout sendDataHeader;
	private LinearLayout sendDataView;
	private LinearLayout receiveDataHeader;
	private TextView receiveDataView;
	private CheckBox checkSend;
	private CheckBox checkReceive;
	private ImageView expandSend;
	private ImageView expandReceive;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

	}

	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_transmission;
	}

	@Override
	protected void setupUI(View rootLayout)
	{
		sendDataHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.sendHeader);
		sendDataView = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.sendSettings);
		sendDataHeader.setOnClickListener(this);

		receiveDataHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.receiveHeader);
		receiveDataView = (TextView) ((ViewGroup) rootLayout).findViewById(R.id.receiveSettings);
		receiveDataHeader.setOnClickListener(this);

		checkSend = (CheckBox) ((ViewGroup) rootLayout).findViewById(R.id.checkSend);
		checkSend.setOnClickListener(this);
		checkReceive = (CheckBox) ((ViewGroup) rootLayout).findViewById(R.id.checkReceive);
		checkReceive.setOnClickListener(this);

		// on old Android versions the label overlaps the checkbox
		if(android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.JELLY_BEAN_MR1)
		{
			addChbxPadding(checkSend);
			addChbxPadding(checkReceive);
		}

		expandSend = (ImageView) ((ViewGroup) rootLayout).findViewById(R.id.expandSend);
		expandReceive = (ImageView) ((ViewGroup) rootLayout).findViewById(R.id.expandReceive);
	}

	/**
	 * Handles layout changes when views are expanded
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void toggleView(View clickview, View expandview)
	{
		expandview.setVisibility(expandview.isShown() ? View.GONE : View.VISIBLE);
		clickview.setBackgroundResource(expandview.isShown() ? R.layout.drop_shadow_top : R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1))
				.setImageResource(expandview.isShown() ? R.drawable.ic_action_collapse : R.drawable.ic_action_expand);
	}

	/**
	 * Makes view non-expandable
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void disableToggle(View clickview, View expandview)
	{
		expandview.setVisibility(View.GONE);
		clickview.setBackgroundResource(R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1)).setVisibility(View.GONE);
	}

	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
		case R.id.sendHeader:
			toggleView(sendDataHeader, sendDataView);
			break;
		case R.id.receiveHeader:
			toggleView(receiveDataHeader, receiveDataView);
			break;
		case R.id.checkSend:
			if(checkSend.isChecked())
			{
				expandSend.setVisibility(View.VISIBLE);
				toggleView(sendDataHeader, sendDataView);
			}
			else
			{
				disableToggle(sendDataHeader, sendDataView);
			}
			break;
		case R.id.checkReceive:
			if(checkReceive.isChecked())
			{
				expandReceive.setVisibility(View.VISIBLE);
				toggleView(receiveDataHeader, receiveDataView);
			}
			else
			{
				disableToggle(receiveDataHeader, receiveDataView);
			}
			break;
		}
	}

	/**
	 * @param checkbox
	 * @see http://stackoverflow.com/questions/4037795/android-spacing-between-checkbox-and-text
	 */
	private void addChbxPadding(CheckBox checkbox)
	{
		checkbox.setPadding(checkbox.getPaddingLeft() + (int) (30.0f * this.getResources().getDisplayMetrics().density + 0.5f), checkbox.getPaddingTop(), checkbox.getPaddingRight(), checkbox.getPaddingBottom());
	}

	@Override
	public void onResume()
	{
		super.onResume();
		if(checkSend.isChecked())
			((ImageView) ((ViewGroup) sendDataHeader).getChildAt(1)).setVisibility(View.VISIBLE);
		if(checkReceive.isChecked())
			((ImageView) ((ViewGroup) receiveDataHeader).getChildAt(1)).setVisibility(View.VISIBLE);
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_transmission);
	}
	
}
