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
import org.apache.http.client.methods.HttpGet;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.util.DeviceControl;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.util.Log;

/**
 * Background Async Task to download files
 * 
 * @author Michalis Vitos, mstevens
 */
public class AsyncDownloader extends AsyncTask<Void, Integer, Boolean>
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
		new AsyncDownloader(context, downloadFolder, downloadUrl, callback).execute();
	}
	
	// DYNAMICS ----------------------------------------------------------
	private final Context context;
	private final String downloadUrl;
	private final Callback callback;
	private final File downloadedFile;
	private final ProgressDialog progressDialog;
	
	private Exception failure;

	private AsyncDownloader(Context context, File downloadFolder, String downloadUrl, Callback callback)
	{
		if(callback == null)
			throw new NullPointerException("Callback cannot be null!");
		this.context = context;
		this.downloadUrl = downloadUrl;
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
	protected Boolean doInBackground(Void... voids)
	{
		if(DeviceControl.isOnline(context))
		{
			int count;
			try
			{
				URL url = new URL(downloadUrl);
				HttpURLConnection connection = (HttpURLConnection) url.openConnection();
				connection.setRequestMethod(HttpGet.METHOD_NAME);
				connection.connect();
				// getting file length
				int fileLength = connection.getContentLength();

				// input stream to read file - with 8k buffer
				InputStream input = new BufferedInputStream(url.openStream(), 8192);
				// Output stream to write file
				OutputStream output = new FileOutputStream(downloadedFile);

				byte data[] = new byte[1024];
				long total = 0;
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
		// Dismiss the dialog after the file was downloaded
		progressDialog.dismiss();
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