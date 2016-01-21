/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.loopj.android.http.RequestParams;
import com.loopj.android.http.SyncHttpClient;

import android.util.Log;
import cz.msebera.android.httpclient.Header;
import cz.msebera.android.httpclient.message.BasicHeader;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.VersionComparator;
import uk.ac.ucl.excites.sapelli.shared.util.android.http.ChainableRequestParams;
import uk.ac.ucl.excites.sapelli.shared.util.android.http.ResponseHandler;
import uk.ac.ucl.excites.sapelli.storage.model.Attachment;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.TimeStampUtils;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;

/**
 * @author mstevens
 *
 */
public class AndroidGeoKeyClient extends GeoKeyClient
{
	
	// STATIC -----------------------------------------------------------------
	static private enum AuthMode
	{
		RequestParam,
		Header,
		Both
	};
	
	static private final AuthMode AUTH_MODE = AuthMode.Header;
	
	static private SyncHttpClient HttpClient = new SyncHttpClient();
	
	static private final String TAG = AndroidGeoKeyClient.class.getSimpleName();
	
	// DYNAMIC ----------------------------------------------------------------
	private final CollectorApp app;
		
	private GeoKeyAccount account;
	
	public AndroidGeoKeyClient(CollectorApp app)
	{
		super(app.collectorClient);
		this.app = app;
	}

	@Override
	protected File getTempFolder()
	{
		return app.getFileStorageProvider().getTempFolder(true);
	}
	
	/**
	 * @param account only used to get server URL (no log-in is performed)
	 * @return
	 */
	public boolean verifyServer(GeoKeyAccount account)
	{
		ResponseHandler handler = getWithJSONResponse(
			getAbsoluteUrl(account, PATH_API_GEOKEY + "info/"),
			null,
			null);
		if(!checkResponseObject(handler, JSON_KEY_GEOKEY))
		{
			logError(handler, "Could not verify server");
			return false;
		}
		//else:
		JSONObject gkInfo = handler.getResponseObject().optJSONObject(JSON_KEY_GEOKEY);
		// Check GK version:
		if(!gkInfo.has(JSON_KEY_VERSION) || !VersionComparator.isAtLeast(gkInfo.optString(JSON_KEY_VERSION), MINIMAL_GEOKEY_VERSION))
			return false;
		// Check extension presence & version:
		JSONArray extensions = gkInfo.optJSONArray(JSON_KEY_INSTALLED_EXTENSIONS);
		if(extensions != null)
			for(int i = 0; i < extensions.length(); i++)
			{
				JSONObject extension = extensions.optJSONObject(i);
				if(extension == null)
					continue;
				if(EXTENSION_GEOKEY_SAPELLI.equals(extension.optString(JSON_KEY_NAME)))
					return VersionComparator.isAtLeast(extension.optString(JSON_KEY_VERSION), MINIMAL_GEOKEY_SAPELLI_VERSION);
			}
		return false;
	}
	
	/**
	 * @return
	 */
	@Override
	public boolean login(GeoKeyAccount account)
	{
		if(account.equals(this.account) && isLoggedIn())
			return true; // given user is still logged in
		
		// Perform new login...
		this.account = account; // !!!
		ResponseHandler handler = postWithJSONResponse(
			getAbsoluteUrl(this.account, PATH_API_GEOKEY_SAPELLI + "login/"),
			null,
			getNewRequestParams(null)
				.putt("username", account.getEmail())
				.putt("password", account.getPassword()));
		if(!checkResponseObject(handler, JSON_KEY_ACCESS_TOKEN))
		{
			logError(handler, "Failed to log in (username/email: " + account.getEmail() + ")"); 
			return false;
		}

		// Store token in account:
		JSONObject token = handler.getResponseObject();
		account.setToken(token.toString());
		
		// Get user display name:
		handler = getWithJSONResponse(
			getAbsoluteUrl(this.account, PATH_API_GEOKEY + "user/"), 
			getNewHeaders(token),
			null);
		if(!checkResponseObject(handler, JSON_KEY_USER_DISPLAY_NAME))
			logError(handler, "Failed to get user info");
		
		// Store display name in account:
		account.setUserDisplayName(handler.getResponseObject().optString(JSON_KEY_USER_DISPLAY_NAME, null));
		
		return true;
	}
	
	@Override
	public boolean isLoggedIn()
	{
		return getAccountToken(true, null) != null;
	}
	
	/**
	 * @param checkValidity whether or not to check if the token is still valid (entails communication with the server)
	 * @param errorMsg may be null
	 * @return a token or {@code null}
	 */
	protected JSONObject getAccountToken(boolean checkValidity, String errorMsg)
	{
		JSONObject token = null;
		
		// Try to get token from account:
		if(account != null && account.hasToken())
			try
			{
				token = new JSONObject(account.getToken());
			}
			catch(JSONException ignore) {}
		
		// Check if token is still valid for at least another hour:
		if(checkValidity && token != null)
		{
			TimeStamp tokenExpires = null;
			try
			{
				tokenExpires = new TimeStamp(TimeUtils.ISOWithMSFormatter.withOffsetParsed().parseDateTime(token.optString("expires_at")));
			}
			catch(Exception ignore) {}
			if(tokenExpires == null || tokenExpires.isBefore(TimeStamp.now().shift(TimeUtils.ONE_HOUR_MS)))
				token = null;
		}
		
		// Final check, ask the server:
		if(checkValidity && token != null)
		{
			ResponseHandler handler = getWithJSONResponse(
				getAbsoluteUrl(account, PATH_API_GEOKEY_SAPELLI + "login/"),
				getNewHeaders(token),
				null);
			if(!checkResponseObject(handler, JSON_KEY_LOGGED_IN) || !handler.getResponseObject().optBoolean(JSON_KEY_LOGGED_IN, false))
				token = null;
		}
		
		// Report back:
		if(token != null)
			return token;
		else
		{
			if(errorMsg != null)
				Log.e(TAG, errorMsg);
			logout();
			return null;
		}
	}
	
	@Override
	public void logout()
	{
		if(account != null)
		{
			if(account.hasToken())
				account.setToken(null);
			account = null;
		}
	}
	
	@Override
	public GeoKeyAccount getAccount()
	{
		return account;
	}
	
	@Override
	protected ProjectSession getModelSession(long modelID) throws UnknownModelException
	{
		Project project = app.collectorClient.getProject(modelID);
		if(project == null)
		{
			Log.e(TAG, "Cannot open ProjectSession because no Project was found for the given Model.");
			return null;
		}
		
		return getProjectSession(project);
	}
	
	/**
	 * @param project should not be {@code null}
	 * @return whether of not the server has a matching Sapelli project to which the logged-in user has contribution access
	 * @throws IllegalStateException when the use r is not/no longer logged in
	 */
	public boolean doesServerHaveProjectForContribution(Project project) throws IllegalStateException
	{
		ProjectSession session = null;
		try
		{
			session = getProjectSession(project);
		}
		catch(UnknownModelException ume)
		{
			return false;
		}
		if(session == null)
			throw new IllegalStateException("User no longer logged in");
		return true;
	}
	
	/**
	 * @param project
	 * @return a {@link ProjectSession} instance, or {@code null} if the user is not or no longer logged in
	 * @throws UnknownModelException when the server has no matching Sapelli project to which the logged-in user has contribution access
	 */
	protected ProjectSession getProjectSession(Project project) throws UnknownModelException
	{
		JSONObject token = getAccountToken(false, "Cannot open ProjectSession because user is not logged in.");
		if(token == null)
			return null;
		
		// Request project description:
		ResponseHandler handler = getWithJSONResponse(
			getAbsoluteUrl(account, PATH_API_GEOKEY_SAPELLI + "projects/description/" + project.getID() + "/" + project.getFingerPrint() + "/"),
			getNewHeaders(token),
			null);
		if(checkResponseObject(handler, JSON_KEY_GEOKEY_PROJECT_ID))
		{
			//Log.d(TAG, "ProjectInfo: " + handler.getResponseObject().toString());
			return new ProjectSession(project, handler.getResponseObject());
		}
		else
		{
			if(handler.hasResponseObject() && JSON_VALUE_ERROR_NO_SUCH_PROJECT.equalsIgnoreCase(handler.getResponseObject().optString(JSON_KEY_ERROR)))
				throw new UnknownModelException(project.getModel().id, project.getModel().name);
			logError(handler, "Could not get project description");
			return null;
		}
	}
	
	/**
	 * Example of project description JSON:
	 * 
	 * <pre><code>
		{
			"sapelli_project_fingerprint": 314654538,
			"sapelli_project_id": 1234,
			"geokey_project_name": "Transport Demo (v2.3)",
			"sapelli_project_variant": null,
			"sapelli_project_name": "Transport Demo",
			"sapelli_model_id": 5279027149407442,
			"sapelli_project_version": "2.3",
			"geokey_project_id": 148,
			"sapelli_project_forms":
			[
				{
					"sapelli_model_schema_number": 1,
					"geokey_category_id": 558,
					"sapelli_form_id": "Survey"
				}
			]
		}
	</code></pre>
	 * 
	 * @author mstevens
	 */
	public class ProjectSession implements ModelSession
	{
		
		public final Project project;
		protected final int geokeyProjectID;
		protected final Map<Form, Integer> form2gkCategoryID;
		
		public ProjectSession(Project project, JSONObject gksProjectInfo)
		{
			this.project = project;
			this.geokeyProjectID = gksProjectInfo.optInt(JSON_KEY_GEOKEY_PROJECT_ID);
			this.form2gkCategoryID = new HashMap<Form, Integer>(project.getNumberOfForms());
			JSONArray formMappings = gksProjectInfo.optJSONArray("sapelli_project_forms");
			if(formMappings != null)
				for(int i = 0; i < formMappings.length(); i++)
				{
					JSONObject formMapping = formMappings.optJSONObject(i);
					if(formMapping == null)
						continue;
					Form form = project.getForm(formMapping.optString("sapelli_form_id"));
					int gkCategoryID = formMapping.optInt("geokey_category_id", -1);
					if(form != null && gkCategoryID != -1)
						form2gkCategoryID.put(form, gkCategoryID);
				}
		}
		
		protected String getSapelliProjectURL()
		{
			return PATH_API_GEOKEY_SAPELLI + "projects/" + geokeyProjectID + "/";
		}
		
		protected String getGeoKeyProjectURL()
		{
			return PATH_API_GEOKEY + "projects/" + geokeyProjectID + "/";
		}
		
		/**
		 * @param csvFile
		 * @param deleteUponSuccess
		 * @param deleteUponFailure
		 * @return whether or not uploading was successful
		 */
		@Override
		public boolean uploadCSV(File csvFile, boolean deleteUponSuccess, boolean deleteUponFailure)
		{
			// Check if we have an account & token:
			JSONObject token = getAccountToken(false, "Cannot upload CSV because user is not logged in.");
			if(token == null)
				return false;
			
			ResponseHandler handler = postWithJSONResponse(
					getAbsoluteUrl(account, getSapelliProjectURL() + "csv_upload/"),
					getNewHeaders(token),
					getNewRequestParams(token).putt(PARAMETER_KEY_CVS_FILE, csvFile, "text/csv"));
			if(checkResponseObject(handler, JSON_KEY_ADDED))
			{
				JSONObject json = handler.getResponseObject();
				Log.d(	TAG,
						String.format(
							"Uploaded CSV file: %d records added; %d records updated; %d duplicates ignored; %d location-less records ignored",
							json.optInt(JSON_KEY_ADDED),
							json.optInt(JSON_KEY_UPDATED),
							json.optInt(JSON_KEY_IGNORED_DUPS),
							json.optInt(JSON_KEY_IGNORED_NO_LOC)));
				if(deleteUponSuccess)
					FileUtils.deleteQuietly(csvFile);
				return true;
			}
			else
			{
				if(deleteUponFailure)
					FileUtils.deleteQuietly(csvFile);
				logError(handler, "Could not upload CSV file");
				return false;
			}
		}
		
		/**
		 * @param record
		 * @param attachments
		 * @return whether or not uploading was successful
		 */
		@Override
		public boolean uploadAttachments(Record record, List<? extends Attachment> attachments)
		{
			// Check if we have an account & token:
			JSONObject token = getAccountToken(false, "Cannot upload attachments because user is not logged in.");
			if(token == null)
				return false;
			
			// Arguments check:
			if(record == null || attachments == null)
				return false;
			
			// Get Form:
			Form form = project.getForm(record.getSchema());
			if(form == null)
			{
				Log.e(TAG, "No Form with " + record.getSchema().toString() + " found in project " + project.toString(false));
				return false;
			}
			
			// Get contribution id:
			Integer gkCategoryID = form2gkCategoryID.get(form);
			if(gkCategoryID == null)
			{
				Log.e(TAG, "Server has no GeoKey category for form " + form.id);
				return false;
			}
			ResponseHandler handler = getWithJSONResponse(
				getAbsoluteUrl(account, getSapelliProjectURL() + "find_observation/" + gkCategoryID.toString() + "/" + TimeStampUtils.getISOTimestamp(Form.GetStartTime(record), true) + "/" + Long.toString(Form.GetDeviceID(record)) + "/"),
				getNewHeaders(token),
				null);
			if(!checkResponseObject(handler, JSON_KEY_OBSERVATION_ID))
			{
				logError(handler, "Could not get contribution ID for record (PK: " + record.getReference().toString() + ")"); // no such Project or observation
				return false;
			}
			//else:
			int contribution_id = handler.getResponseObject().optInt(JSON_KEY_OBSERVATION_ID);
			String contributionMediaURL = getAbsoluteUrl(account, getGeoKeyProjectURL() + "contributions/" + Integer.toString(contribution_id) + "/media/");
			
			// Get existing documents:
			handler = getWithJSONResponse(contributionMediaURL, getNewHeaders(token), null);
			if(!checkResponseArray(handler))
			{
				logError(handler, "Could not get documents list for contribution with ID " + contribution_id);
				return false;
			}
			//else:
			JSONArray existingDocuments = handler.getResponseArray();
			List<String> existingDocumentNames = new ArrayList<String>(existingDocuments.length());
			for(int i = 0; i < existingDocuments.length(); i++)
			{
				JSONObject document = existingDocuments.optJSONObject(i);
				if(document != null && document.has(JSON_KEY_NAME))
					existingDocumentNames.add(document.optString(JSON_KEY_NAME));
			}
				
			// Upload files that don't exist on the server:
			boolean failure = false;
			for(Attachment attachment : attachments)
			{
				if(!FileHelpers.isReadableFile(attachment.file))
					continue; // file does not exist or is not readable
				String name = FileHelpers.trimFileExtensionAndDot(attachment.file.getName());
				if(existingDocumentNames.contains(name))
					continue; // file already exists on server
				// Upload file:
				handler = postWithJSONResponse(
					contributionMediaURL,
					getNewHeaders(token),
					getNewRequestParams(token)
						.putt(PARAMETER_KEY_FILE, attachment.file, attachment.getMimeType())
						.putt(PARAMETER_KEY_NAME, name)
						.putt(PARAMETER_KEY_DESC, app.getString(R.string.uploadedFromAt, app.getBuildInfo().getNameAndVersion(), TimeStampUtils.getISOTimestamp(TimeStamp.now(), true))));
				if(!checkResponseObject(handler, JSON_KEY_NAME))
				{
					logError(handler, "Could not upload media file: " + attachment.file.getName());
					failure = true;
				}
			}
			
			// Return success indicator:
			return !failure;
		}
		
	}
	
	protected boolean checkResponseObject(ResponseHandler handler)
	{
		return checkResponseObject(handler, null);
	}
	
	private boolean checkResponseObject(ResponseHandler handler, String jsonKey)
	{
		return	!handler.hasError() &&
				handler.hasResponseObject() &&
				(jsonKey == null || handler.getResponseObject().has(jsonKey));
	}
	
	private boolean checkResponseArray(ResponseHandler handler)
	{
		return	!handler.hasError() &&
				handler.hasResponseArray();
	}
	
	private void logError(ResponseHandler handler, String msg)
	{
		String logMsg = msg +
			(handler.hasResponseObject() && handler.getResponseObject().has(JSON_KEY_ERROR) ?
				"; server response: " + handler.getResponseObject().optString(JSON_KEY_ERROR) : "");
		if(handler.hasError())
			Log.e(TAG, logMsg, handler.getError());
		else
			Log.e(TAG, logMsg);
	}
	
	private String getAbsoluteUrl(GeoKeyAccount account, String relativeUrl)
	{
		return account.getUrl() + relativeUrl;
	}
	
	private ChainableRequestParams getNewRequestParams(JSONObject token)
	{
		ChainableRequestParams params = new ChainableRequestParams();
		if(token != null && (AUTH_MODE == AuthMode.RequestParam || AUTH_MODE == AuthMode.Both))
			params.put(JSON_KEY_ACCESS_TOKEN, token.optString(JSON_KEY_ACCESS_TOKEN));
		return params;
	}
	
	private List<Header> getNewHeaders(JSONObject token)
	{
		List<Header> headerList = new ArrayList<Header>();
		if(token != null && (AUTH_MODE == AuthMode.Header || AUTH_MODE == AuthMode.Both))
			headerList.add(new BasicHeader("Authorization", token.optString(JSON_KEY_TOKEN_TYPE) + " " + token.optString(JSON_KEY_ACCESS_TOKEN)));
		return headerList;
	}
	
	private ResponseHandler getWithJSONResponse(String absoluteUrl, List<Header> headers, RequestParams params)
	{
		ResponseHandler response = new ResponseHandler();
		// Blocking!:
		HttpClient.get(app, absoluteUrl, toArray(headers), params, response);
		return response;
	}

	private ResponseHandler postWithJSONResponse(String absoluteUrl, List<Header> headers, RequestParams params)
	{
		ResponseHandler handler = new ResponseHandler();
		// Blocking!:
		HttpClient.post(app, absoluteUrl, toArray(headers), params, null /*contentType*/, handler);
		return handler;
	}
	
	private Header[] toArray(List<Header> headers)
	{
		if(headers == null)
			return null;
		return headers.toArray(new Header[headers.size()]);
	}
	
}
