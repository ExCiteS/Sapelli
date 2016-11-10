/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import android.content.Context;
import android.graphics.Color;
import android.support.v4.content.ContextCompat;
import android.view.View;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.RelativeLayout;

import timber.log.Timber;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.HtmlField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;


/**
 * @author Michalis Vitos
 *
 */
public class AndroidHtmlUI extends HtmlUI<View, CollectorView>
{
	private RelativeLayout backgroundView;

	public AndroidHtmlUI(HtmlField htmlField, AndroidCollectorController controller, CollectorView collectorView)
	{
		super(htmlField, controller, collectorView);
	}

	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if (backgroundView == null)
		{
			Context context = collectorUI.getContext();
			backgroundView = new RelativeLayout(context);
			backgroundView.setBackgroundColor(ContextCompat.getColor(context, R.color.blue_sap));

			RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, RelativeLayout.LayoutParams.MATCH_PARENT);
			params.addRule(RelativeLayout.CENTER_IN_PARENT);


			// Webview
			WebView webView = new WebView(context);
			webView.getSettings().setJavaScriptEnabled(true);
			webView.setWebViewClient(new WebViewClient());
			webView.setBackgroundColor(Color.argb(1, 0, 0, 0));
			webView.getSettings().setDomStorageEnabled(true);
			webView.getSettings().setDatabaseEnabled(true);
			if (true)
			{
				webView.getSettings().setAppCacheEnabled(true);
				webView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
			}
			//final String url = "file:///storage/emulated/0/Download/planted.html";
			final String url = "file://" + controller.getFileStorageProvider().getProjectResFile(controller.getCurrentForm().getProject(), "planted.html");
			webView.loadUrl(url);
			Timber.d("Try to load: %s", url);

			// View
			View view = new View(context);
			view.setBackgroundColor(ContextCompat.getColor(context, R.color.colorPrimary));

			backgroundView.addView(webView, params);
		}

		return backgroundView;
	}

}
