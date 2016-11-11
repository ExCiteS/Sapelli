/**
 * Sapelli data collection platform: http://sapelli.org
 * <p>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.view.LayoutInflater;
import android.view.View;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;

import org.apache.commons.validator.routines.UrlValidator;

import java.util.Locale;

import timber.log.Timber;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.HtmlField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;


/**
 * @author Michalis Vitos
 */
public class AndroidHtmlUI extends HtmlUI<View, CollectorView>
{
	private RelativeLayout backgroundView;
	private WebView webView;
	private ProgressBar progressBar;

	public AndroidHtmlUI(HtmlField htmlField, AndroidCollectorController controller, CollectorView collectorView)
	{
		super(htmlField, controller, collectorView);
	}

	@SuppressLint("SetJavaScriptEnabled")
	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(backgroundView == null)
		{
			final Context context = collectorUI.getContext();
			final LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);

			// Get views
			backgroundView = (RelativeLayout) inflater.inflate(R.layout.field_html_ui, null);
			webView = (WebView) backgroundView.findViewById(R.id.webview);
			progressBar = (ProgressBar) backgroundView.findViewById(R.id.webview_progressbar);

			// Webview
			webView.getSettings().setJavaScriptEnabled(true);
			webView.setWebViewClient(new WebClient());
			webView.getSettings().setDomStorageEnabled(true);
			webView.getSettings().setDatabaseEnabled(true);
			webView.getSettings().setAppCacheEnabled(true);
			webView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);

			// Build URL
			UrlValidator urlValidator = new UrlValidator();
			String url = "";
			try
			{
				if(urlValidator.isValid(field.getUrl()))
				{
					url = field.getUrl();
				}
				else
				{
					String localFile = controller.getFileStorageProvider()
					  .getProjectResFile(
						controller.getCurrentForm().getProject(),
						field.getUrl()
					  )
					  .toString();

					// file://local_file
					url = String.format(Locale.getDefault(), "file://%s", localFile);
				}
			}
			catch(Exception e)
			{
				Timber.e(e, "Error in URL builder: ");
			}

			webView.loadUrl(url);
		}

		return backgroundView;
	}

	@Override
	public void goBack()
	{
		if(webView != null)
			webView.goBack();
	}

	@Override
	public boolean canGoBack()
	{
		return webView != null && webView.canGoBack();
	}

	/**
	 * Custom WebView client
	 *
	 * @author Michalis Vitos
	 */
	private class WebClient extends WebViewClient
	{
		private String url;

		@Override
		public void onPageStarted(WebView view, String url, Bitmap favicon)
		{
			this.url = url;
			Timber.d("Started loading URL: %s", url);

			// Show progressbar
			if(progressBar != null)
				progressBar.setVisibility(View.VISIBLE);

			super.onPageStarted(view, url, favicon);
		}

		@Override
		public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request)
		{
			Timber.d("Override request: %s", request);
			return super.shouldOverrideUrlLoading(view, request);
		}

		@Override
		public void onPageFinished(WebView webview, String url)
		{
			Timber.d("Finished loading URL: %s", url);

			// Show progressbar
			if(progressBar != null)
				progressBar.setVisibility(View.GONE);

			super.onPageFinished(webview, url);
		}

		@Override
		public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error)
		{
			Timber.d("Error loading URL: %s", url);

			// Show progressbar
			if(progressBar != null)
				progressBar.setVisibility(View.GONE);

			super.onReceivedError(view, request, error);
		}
	}
}
