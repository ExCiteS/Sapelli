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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.os.Bundle;
import android.text.Html;
import android.text.method.LinkMovementMethod;
import android.view.View;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;

/**
 * @author mstevens
 *
 */
public class AboutFragment extends ProjectManagerFragment
{

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.fragment_about;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(android.view.View)
	 */
	@Override
	protected void setupUI(View rootLayout)
	{
		CollectorApp app = getOwner().getCollectorApp();
		DeviceID deviceID = getOwner().getDeviceID();
		
		TextView infoLbl = (TextView) rootLayout.findViewById(R.id.aboutInfo);
		infoLbl.setClickable(true);
		infoLbl.setMovementMethod(LinkMovementMethod.getInstance());
		infoLbl.setText(Html.fromHtml(
				"<p><b>" + app.getBuildInfo().getNameAndVersion() + "</b><br/>[" + app.getBuildInfo().getExtraVersionInfo() + "]</p>" +
				"<p>" + app.getBuildInfo().getBuildInfo() + ".</p>" +
				"<p>" + getString(R.string.by_ucl_excites_html)  + "</p>" + 
				"<p>" + getString(R.string.license)  + "</p>" +
				"<p>" + "Device ID (CRC32): " + (deviceID != null ? deviceID.getIDAsCRC32Hash() : "?") + ".</p>"));
	}

	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(R.drawable.ic_sapelli_logo)
		.setTitle(R.string.about)
		.setPositiveButton(android.R.string.ok, null); 
		AlertDialog dialog = builder.create();
		
		// Set view:
		setDialogView(dialog);

		return dialog;
	}
	
}
