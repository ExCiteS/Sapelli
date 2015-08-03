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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.apache.commons.io.FileUtils;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.util.Log;

/**
 * Background Async Task to download files
 * 
 * Note: This implementation cannot be safely used in combination with an activity that is restarted upon screen orientation changes! More info: -
 * http://stackoverflow.com/questions/18214293 - http://stackoverflow.com/questions/20279216/asynctaskloader-basic-example-android
 * 
 * @author Michalis Vitos, mstevens
 */
public class AsyncDownloader extends AsyncTask<String, Integer, Boolean>
{

	// STATICS -----------------------------------------------------------
	static private final String TAG = "AsyncDownloader";
	static private final String TEMP_FILE_EXTENSION = "tmp";

	/**
	 * Downloads a file and reports back via callback
	 * 
	 * @param context
	 * @param downloadFolder
	 * @param downloadUrl
	 * @param callback
	 */
	static public void Download(Context context, File downloadFolder, String downloadUrl, Callback callback)
	{
		new AsyncDownloader(context, downloadFolder, callback).execute(downloadUrl);
	}

	// DYNAMICS ----------------------------------------------------------
	private final Context context;
	private final Callback callback;
	private final File downloadedFile;
	private final ProgressDialog progressDialog;

	private String downloadUrl;
	private Exception failure;

	private AsyncDownloader(Context context, File downloadFolder, Callback callback)
	{
		if(callback == null)
			throw new NullPointerException("Callback cannot be null!");
		this.context = context;
		this.callback = callback;

		// Download file in folder /Downloads/timestamp-filename
		downloadedFile = new File(downloadFolder.getAbsolutePath() + File.separator + (System.currentTimeMillis() / 1000) + '.' + TEMP_FILE_EXTENSION);

		// Set-up progess dialog:
		progressDialog = new ProgressDialog(context);
		progressDialog.setMessage(context.getString(R.string.downloading));
		progressDialog.setIndeterminate(false);
		progressDialog.setMax(100);
		progressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
		progressDialog.setCancelable(true);
	}

	/**
	 * Show Progress Bar Dialog before starting the downloading
	 * */
	@Override
	protected void onPreExecute()
	{
		super.onPreExecute();
		progressDialog.setButton(DialogInterface.BUTTON_POSITIVE, context.getString(android.R.string.cancel), new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int which)
			{
				AsyncDownloader.this.cancel(true);
				// Delete the downloaded file
				downloadedFile.delete();
			}
		});
		progressDialog.show();
	}

	/**
	 * Downloading file in background thread
	 * 
	 * @return
	 * */
	@Override
	protected Boolean doInBackground(String... params)
	{
		downloadUrl = params[0];
		Log.d(getClass().getSimpleName(), "Download URL: " + downloadUrl);
		return download(downloadUrl);
	}

	private boolean download(String downloadUrl)
	{
		if(DeviceControl.isOnline(context))
		{
			try
			{
				URL url = new URL(downloadUrl);
				HttpURLConnection conn = (HttpURLConnection) url.openConnection();
				conn.setRequestMethod("GET");
				conn.setInstanceFollowRedirects(false); // we handle redirects manually below (otherwise HTTP->HTTPS redirects don't work):
				conn.connect();

				// Detect & follow redirects:
				int status = conn.getResponseCode();
				Log.d(getClass().getSimpleName(), "Response Code: " + status);
				if(status == HttpURLConnection.HTTP_MOVED_TEMP || status == HttpURLConnection.HTTP_MOVED_PERM || status == HttpURLConnection.HTTP_SEE_OTHER)
				{	// follow redirect url from "location" header field
					String newUrl = conn.getHeaderField("Location");
					Log.d(getClass().getSimpleName(), "Redirect to URL : " + newUrl);
					return download(newUrl);
				}

				// getting file length
				int fileLength = conn.getContentLength();

				// input stream to read file - with 8k buffer
				InputStream input = new BufferedInputStream(url.openStream(), 8192);
				// Output stream to write file
				OutputStream output = new FileOutputStream(downloadedFile);

				byte data[] = new byte[1024];
				long total = 0;
				int count;
				while((count = input.read(data)) != -1)
				{
					total += count;
					// Publish the progress....
					publishProgress((int) (total * 100 / fileLength));

					// writing data to file
					output.write(data, 0, count);
				}

				// flushing output
				output.flush();

				// closing streams
				output.close();
				input.close();
			}
			catch(Exception e)
			{
				failure = e;
				return false;
			}
			return true;
		}
		failure = new Exception("The device is not online");
		return false;
	}

	protected void onProgressUpdate(Integer... progress)
	{
		progressDialog.setProgress(progress[0]); // update progress bar
	}

	@Override
	protected void onPostExecute(Boolean downloadFinished)
	{
		// Dismiss the dialog after the file was downloaded:
		try
		{
			progressDialog.dismiss(); // Note: this fails if activity has been restarted in the meantime!
		}
		catch(Exception e)
		{
			Log.e(this.getClass().getSimpleName(), "Error upon dismissing progress dialog, likely due to Activity restart", e);
		}
		// Report success or failure:
		if(downloadFinished)
			callback.downloadSuccess(downloadUrl, downloadedFile);
		else
		{
			// Delete (partially) downloaded file:
			FileUtils.deleteQuietly(downloadedFile);
			// Report problem:
			Log.e(TAG, "Download problem", failure);
			callback.downloadFailure(downloadUrl, failure);
		}
	}

	/**
	 * Callback methods to report on download success/failure
	 * 
	 * @author mstevens
	 */
	public interface Callback
	{

		public void downloadSuccess(String downloadUrl, File downloadedFile);

		public void downloadFailure(String downloadUrl, Exception cause);

	}

}