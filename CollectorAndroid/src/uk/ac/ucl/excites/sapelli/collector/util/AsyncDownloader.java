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
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.apache.commons.io.FileUtils;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import android.annotation.TargetApi;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.os.Build;
import android.util.Log;

/**
 * Background Async Task to download files
 * 
 * Note:
 * 	This implementation cannot be safely used in combination with an activity that is restarted upon screen orientation changes! TODO fix this!
 * 
 * 	More info:	- http://stackoverflow.com/questions/18214293
 * 				- http://stackoverflow.com/questions/20279216/asynctaskloader-basic-example-android
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

		// Set-up progress dialog:
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
				FileUtils.deleteQuietly(downloadedFile);
			}
		});
		// Don't show dialog yet!
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
		//Log.d(getClass().getSimpleName(), "Download URL: " + downloadUrl);
		return download(downloadUrl);
	}

	private boolean download(String downloadUrl)
	{
		if(DeviceControl.isOnline(context))
		{
			InputStream input = null;
			OutputStream output = null;
			try
			{
				URL url = new URL(downloadUrl);
				HttpURLConnection conn = (HttpURLConnection) url.openConnection();
				conn.setRequestMethod("GET");
				conn.setInstanceFollowRedirects(false); // we handle redirects manually below (otherwise HTTP->HTTPS redirects don't work):
				conn.connect();

				// Detect & follow redirects:
				int status = conn.getResponseCode();
				//Log.d(getClass().getSimpleName(), "Response Code: " + status);
				if(status == HttpURLConnection.HTTP_MOVED_TEMP || status == HttpURLConnection.HTTP_MOVED_PERM || status == HttpURLConnection.HTTP_SEE_OTHER)
				{	// follow redirect url from "location" header field
					String newUrl = conn.getHeaderField("Location");
					//Log.d(getClass().getSimpleName(), "Redirect to URL : " + newUrl);
					return download(newUrl);
				}

				// Getting file length
				final int fileLength = conn.getContentLength();
				publishProgress(fileLength < 0 ? // when fileLength = -1 this means the server hasn't specified the file length
									-1 : // progressDialog will open and be set to indeterminate mode
									0);  // progressDialog will open and be set to 0
				
				// Input stream to read file - with 8k buffer
				input = new BufferedInputStream(url.openStream(), 8192);
				// Output stream to write file
				output = new BufferedOutputStream(new FileOutputStream(downloadedFile));

				byte data[] = new byte[1024];
				int total = 0;
				int percentage = 0;
				int bytesRead;
				while((bytesRead = input.read(data)) != -1)
				{
					// Complete % completion:
					if(fileLength > 0) // don't divide by 0 and only update progress if we know the fileLength (i.e. != -1)
					{
						int newPercentage = (int) ((total += bytesRead) / ((float) fileLength) * 100f);
						if(newPercentage != percentage)
							publishProgress(percentage = newPercentage);
					}
					
					// Write data to file...
					output.write(data, 0, bytesRead);
				}

				// Flush output:
				output.flush();
			}
			catch(Exception e)
			{
				failure = e;
				return false;
			}
			finally
			{	// Close streams:
				StreamHelpers.SilentClose(input);
				StreamHelpers.SilentClose(output);
			}
			//Log.d(getClass().getSimpleName(), "Download done");
			return true;
		}
		else
		{
			failure = new Exception("The device is not online.");
			return false;
		}
	}

	/**
	 * @param progress a percentage or -1 to signify progress is indeterminable
	 * @see android.os.AsyncTask#onProgressUpdate(java.lang.Object[])
	 */
	@Override
	protected void onProgressUpdate(Integer... progress)
	{
		try
		{
			// Open dialog if needed:
			if(!progressDialog.isShowing())
				progressDialog.show();
			// Update progress:
			if(progress == null || progress.length == 0 || progress[0] < 0)
				progressDialog.setIndeterminate(true);
			else
				progressDialog.setProgress(progress[0]);
		}
		catch(Exception ignore) {} // may happen if activity is restarted
	}

	/**
	 * @see http://stackoverflow.com/a/5102572/1084488
	 * @see https://github.com/ExCiteS/Sapelli/issues/42
	 */
	protected void dismisDialog()
	{
		try
		{
			progressDialog.dismiss(); // Note: this fails if activity has been restarted in the meantime!
		}
		catch(final Exception e)
		{
			Log.e(this.getClass().getSimpleName(), "Error upon dismissing progress dialog, likely due to Activity restart", e);
		}
	}
	
	@Override
	protected void onPostExecute(Boolean downloadFinished)
	{
		// Dismiss the dialog after the file was downloaded:
		dismisDialog();
		
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
	
	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onCancelled(java.lang.Object)
	 */
	@TargetApi(Build.VERSION_CODES.HONEYCOMB)
	@Override
	protected void onCancelled(Boolean result)
	{
		dismisDialog();
		super.onCancelled(result);
	}

	/* (non-Javadoc)
	 * @see android.os.AsyncTask#onCancelled()
	 */
	@Override
	protected void onCancelled()
	{
		dismisDialog();
		super.onCancelled();
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