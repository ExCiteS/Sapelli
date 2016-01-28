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
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer;
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
	
	private GeoKeyServer server;
	
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
	 * @param server only used to get server URL (no log-in is performed)
	 * @return
	 */
	public boolean verifyServer(GeoKeyServer server)
	{
		if(server == null)
			return false;
		ResponseHandler handler = getWithJSONResponse(
			getAbsoluteUrl(server, PATH_API_GEOKEY + "info/"),
			null,
			null);
		if(!checkResponseObject(handler, JSON_KEY_GEOKEY))
		{
			logError(handler, "Could not verify server (" + server.getUrl() + "). Possibly is it not a GeoKey server.");
			return false;
		}
		//else:
		JSONObject gkInfo = handler.getResponseObject().optJSONObject(JSON_KEY_GEOKEY);
		// Check GK version:
		if(!gkInfo.has(JSON_KEY_VERSION) || !VersionComparator.isAtLeast(gkInfo.optString(JSON_KEY_VERSION), MINIMAL_GEOKEY_VERSION))
		{
			Log.e(TAG, "GeoKey version on server (" + server.getUrl() + ") is unknown or too old (minimum is v" + MINIMAL_GEOKEY_VERSION + ")");
			return false;
		}
		// Check extension presence & version:
		JSONArray extensions = gkInfo.optJSONArray(JSON_KEY_INSTALLED_EXTENSIONS);
		if(extensions != null)
			for(int i = 0; i < extensions.length(); i++)
			{
				JSONObject extension = extensions.optJSONObject(i);
				if(extension == null)
					continue;
				if(EXTENSION_GEOKEY_SAPELLI.equals(extension.optString(JSON_KEY_NAME)))
				{
					boolean gksapVersionOK = VersionComparator.isAtLeast(extension.optString(JSON_KEY_VERSION), MINIMAL_GEOKEY_SAPELLI_VERSION);
					if(!gksapVersionOK)
						Log.e(TAG, "GeoKey server (" + server.getUrl() + ") has an outdated version of the " + EXTENSION_GEOKEY_SAPELLI + " extension (minimum is v" + MINIMAL_GEOKEY_SAPELLI_VERSION + ")");
					return gksapVersionOK;
				}
			}
		Log.e(TAG, "GeoKey server (" + server.getUrl() + ") does not have the " + EXTENSION_GEOKEY_SAPELLI + " extension.");
		return false;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient#connect(uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer)
	 */
	@Override
	public boolean connectAndLogin(GeoKeyServer server)
	{
		return connectAndLogin(server, true);
	}
	
	public boolean connectAndLogin(GeoKeyServer server, boolean verifyServer)
	{
		// Check if we are still connected/logged-in:
		if(server.equals(this.server) && (!server.hasUserCredentials() || isUserLoggedIn()))
			return true; // still connected/logged-in

		// Verify server if needed:
		if(verifyServer && !verifyServer(server))
			return false;
		
		// Set server to "connect":
		this.server = server; // !!!

		if(!server.hasUserCredentials())
			return true; // there's no user credentials so we cannot/don't have to log-in
		//else: perform new login...
		ResponseHandler handler = postWithJSONResponse(
			getAbsoluteUrl(server, PATH_API_GEOKEY_SAPELLI + "login/"),
			null,
			getNewRequestParams(null)
				.putt("username", server.getUserEmail())
				.putt("password", server.getUserPassword()));
		if(!checkResponseObject(handler, JSON_KEY_ACCESS_TOKEN))
		{
			if(handler.hasResponseObject() && JSON_VALUE_ERROR_DESCRIPTION_INVALID_CREDENTIALS.equalsIgnoreCase(handler.getResponseObject().optString(JSON_KEY_ERROR_DESCRIPTION)))
				client.logError("Failed to log in due to incorrect user credentials");
			else
				logError(handler, "Failed to log in (email: " + server.getUserEmail() + ")");
			return false;
		}

		// Store token in account:
		JSONObject token = handler.getResponseObject();
		server.setToken(token.toString()); // !!!
		
		// Get user display name:
		handler = getWithJSONResponse(
			getAbsoluteUrl(this.server, PATH_API_GEOKEY + "user/"), 
			getNewHeaders(token),
			null);
		if(!checkResponseObject(handler, JSON_KEY_USER_DISPLAY_NAME))
			logError(handler, "Failed to get user info");
		
		// Store display name in account:
		server.setUserDisplayName(handler.getResponseObject().optString(JSON_KEY_USER_DISPLAY_NAME, null));
		
		return true;
	}
	
	@Override
	public boolean isUserLoggedIn()
	{
		return server.hasUserCredentials() && getUserToken(true, null) != null;
	}
	
	/**
	 * @param checkValidity whether or not to check if the token is still valid (entails communication with the server)
	 * @param errorMsg may be null
	 * @return a token or {@code null}
	 */
	protected JSONObject getUserToken(boolean checkValidity, String errorMsg)
	{
		JSONObject token = null;
		
		// Try to get token from account:
		if(server != null && server.hasUserCredentials() && server.hasUserToken())
			try
			{
				token = new JSONObject(server.getUserToken());
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
				getAbsoluteUrl(server, PATH_API_GEOKEY_SAPELLI + "login/"),
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
		if(server != null && server.hasUserToken())
			server.setToken(null);
	}
	
	@Override
	public void disconnect()
	{
		server = null;
	}
	
	@Override
	public GeoKeyServer getServer()
	{
		return server;
	}
	
	/**
	 * @param modelID
	 * @return a {@link ProjectSession} instance, or {@code null} if the user is not or no longer logged in
	 * @throws UnknownModelException when the server has no matching Sapelli project to which the logged-in user has contribution access
	 * @throws IllegalAccessException when the user does not have access to the project in question
	 */
	@Override
	protected ProjectSession getModelSession(long modelID) throws UnknownModelException, IllegalAccessException
	{
		Project project = app.collectorClient.getProject(modelID);
		if(project == null)
		{
			Log.e(TAG, "Cannot open ProjectSession because no Project was found for the given Model.");
			return null;
		}
		
		if(server == null)
			return null;
		JSONObject token = getUserToken(false, server.hasUserCredentials() ? "Cannot open ProjectSession because user is not logged in." : null);
		if(token == null && server.hasUserCredentials())
			return null;
		
		// Request project description:
		ResponseHandler handler = getWithJSONResponse(
			getAbsoluteUrl(server, PATH_API_GEOKEY_SAPELLI + "projects/description/" + project.getID() + "/" + project.getFingerPrint() + "/"),
			getNewHeaders(token),
			null);
		if(checkResponseObject(handler, JSON_KEY_GEOKEY_PROJECT_ID))
		{
			//Log.d(TAG, "ProjectInfo: " + handler.getResponseObject().toString());
			return new ProjectSession(project, handler.getResponseObject(), token);
		}
		else
		{
			if(handler.hasResponseObject() && JSON_VALUE_ERROR_NO_SUCH_PROJECT.equalsIgnoreCase(handler.getResponseObject().optString(JSON_KEY_ERROR)))
				throw new UnknownModelException(project.getModel().id, project.getModel().name);
			else if(handler.hasResponseObject() && JSON_VALUE_ERROR_PROJECT_ACCESS_DENIED.equalsIgnoreCase(handler.getResponseObject().optString(JSON_KEY_ERROR)))
				throw new IllegalAccessException(JSON_VALUE_ERROR_PROJECT_ACCESS_DENIED);
			else
			{
				logError(handler, "Could not get project description");
				return null;
			}
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
		public final JSONObject token;
		protected final int geokeyProjectID;
		protected final Map<Form, Integer> form2gkCategoryID;
		
		/**
		 * @param project
		 * @param gksProjectInfo
		 * @param token - may be null when user does not have credentials and GeoKey project is public and allows anonymous contributions
		 */
		public ProjectSession(Project project, JSONObject gksProjectInfo, JSONObject token)
		{
			this.project = project;
			this.token = token;
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
			ResponseHandler handler = postWithJSONResponse(
					getAbsoluteUrl(server, getSapelliProjectURL() + "csv_upload/"),
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
				getAbsoluteUrl(server, getSapelliProjectURL() + "find_observation/" + gkCategoryID.toString() + "/" + TimeStampUtils.getISOTimestamp(Form.GetStartTime(record), true) + "/" + Long.toString(Form.GetDeviceID(record)) + "/"),
				getNewHeaders(token),
				null);
			if(!checkResponseObject(handler, JSON_KEY_OBSERVATION_ID))
			{
				logError(handler, "Could not get contribution ID for record (PK: " + record.getReference().toString() + ")"); // no such Project or observation
				return false;
			}
			//else:
			int contribution_id = handler.getResponseObject().optInt(JSON_KEY_OBSERVATION_ID);
			String contributionMediaURL = getAbsoluteUrl(server, getGeoKeyProjectURL() + "contributions/" + Integer.toString(contribution_id) + "/media/");
			
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
				{
					client.logInfo("File \"" + name + "\" already exists on the server, no uploading needed.");
					continue; // file already exists on server
				}
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
	
	private String getAbsoluteUrl(GeoKeyServer server, String relativeUrl)
	{
		return server.getUrl() + relativeUrl;
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
