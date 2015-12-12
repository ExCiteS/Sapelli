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
import java.util.List;

import org.json.JSONException;
import org.json.JSONObject;

import com.loopj.android.http.RequestParams;
import com.loopj.android.http.SyncHttpClient;

import android.content.Context;
import android.util.Log;
import cz.msebera.android.httpclient.Header;
import cz.msebera.android.httpclient.message.BasicHeader;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.VersionComparator;
import uk.ac.ucl.excites.sapelli.shared.util.android.JSONHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.http.ChainableRequestParams;
import uk.ac.ucl.excites.sapelli.shared.util.android.http.ResponseHandler;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;

public class GeoKeySapelliSession
{
	
	// STATIC -----------------------------------------------------------------
	static private enum AuthMode
	{
		RequestParam,
		Header,
		Both
	};
	
	static private final AuthMode DEFAULT_AUTH_MODE = AuthMode.Header;
	
	static private final String PATH_API_GEOKEY = "api/";
	static private final String PATH_API_GEOKEY_SAPELLI = "api/sapelli/";
	
	static private final String MINIMAL_GEOKEY_VERSION = "0.8.2";
	static private final String EXTENSION_GEOKEY_SAPELLI = "geokey_sapelli";
	static private final String MINIMAL_GEOKEY_SAPELLI_VERSION = "0.1.1";
	
	static private final String JSON_KEY_GEOKEY_VERSION = "geokey_version";
	static private final String JSON_KEY_INSTALLED_EXTENSIONS = "installed_extensions";
	static private final String JSON_KEY_ACCESS_TOKEN = "access_token";
	static private final String JSON_KEY_TOKEN_TYPE = "token_type";
	static private final String JSON_KEY_GEOKEY_PROJECT_ID = "geokey_project_id";
	
	static private SyncHttpClient client = new SyncHttpClient();
	
	// DYNAMIC ----------------------------------------------------------------
	private final Context context;
	private final GeoKeyAccount account;
	private JSONObject token;
	
	private final AuthMode authMode = DEFAULT_AUTH_MODE;
	
	public GeoKeySapelliSession(Context context, GeoKeyAccount account)
	{
		this.context = context;
		this.account = account;
		
		// Get token from account:
		try
		{
			if(account.hasToken())
				token = new JSONObject(account.getToken());
		}
		catch(JSONException ignore) {}
	}
	
	public boolean verifyServer()
	{
		// Perform login (blocking!):
		ResponseHandler handler = getWithJSONResponse(PATH_API_GEOKEY + "info/", null, null);
		if(handler.hasError() || !handler.hasResponseObject())
		{
			return false; // TODO throw exception
		}
		JSONObject response = handler.getResponseObject();
		return	response.has(JSON_KEY_GEOKEY_VERSION) &&
				VersionComparator.isAtLeast(response.optString(JSON_KEY_GEOKEY_VERSION), MINIMAL_GEOKEY_VERSION) &&
				response.has(JSON_KEY_INSTALLED_EXTENSIONS) &&
				JSONHelpers.contains(response.optJSONArray(JSON_KEY_INSTALLED_EXTENSIONS), EXTENSION_GEOKEY_SAPELLI);
	}
	
	/**
	 * @return
	 */
	public boolean login()
	{
		if(isLoggedIn())
			return true;
		else
			token = null; // Clear old token
		
		verifyServer();
		
		// Perform login (blocking!):
		ResponseHandler response = 
			postWithJSONResponse(	PATH_API_GEOKEY_SAPELLI + "login/", null,
									getNewRequestParams(false)
										.putt("username", account.getUsername())
										.putt("password", account.getPassword()));
		if(	!response.hasError() &&
			response.hasResponseObject() &&
			response.getResponseObject().has(JSON_KEY_ACCESS_TOKEN))
		{
			Log.d(GeoKeySapelliSession.class.getSimpleName(), "Token: " + response.getResponseObject().toString());
			token = response.getResponseObject();
			account.setToken(token.toString());
			// TODO store account in Tstore
			return true;
		}
		else
		{
			if(response.hasError())
				Log.d(GeoKeySapelliSession.class.getSimpleName(), "Error: " + response.getError().getMessage()); 
			return false;
		}
	}
	
	public boolean isLoggedIn()
	{
		return token != null; 	// TODO verify if token is still valid
	}
	
	public ProjectSession openProjectSession(Project project)
	{
		ResponseHandler response = getWithJSONResponse(PATH_API_GEOKEY_SAPELLI + "projects/description/" + (project.getID() + 1) + "/" + project.getFingerPrint() + "/",
			getNewHeaders(true),
			getNewRequestParams(true));
		if(	!response.hasError() &&
			response.hasResponseObject() &&
			response.getResponseObject().has(JSON_KEY_GEOKEY_PROJECT_ID))
			{
				Log.d(GeoKeySapelliSession.class.getSimpleName(), "ProjectInfo: " + response.getResponseObject().toString());
				return new ProjectSession(project, response.getResponseObject());
			}
			else
			{
				// TODO
				return null;
			}
	}
	
	public class ProjectSession
	{
		
		private final Project project;
		private final JSONObject gksProjectInfo;
		
		public ProjectSession(Project project, JSONObject gksProjectInfo)
		{
			this.project = project;
			this.gksProjectInfo = gksProjectInfo;
		}
		
		public void uploadRecords(List<Record> records)
		{
			
		}
		
		public void uploadAttachments(Record record, List<File> attachments)
		{
			
		}
		
	}
	
	protected ChainableRequestParams getNewRequestParams(boolean requiresAuth)
	{
		ChainableRequestParams params = new ChainableRequestParams();
		if(requiresAuth && (authMode == AuthMode.RequestParam || authMode == AuthMode.Both))
			params.put(JSON_KEY_ACCESS_TOKEN, token.optString(JSON_KEY_ACCESS_TOKEN));
		return params;
	}
	
	protected List<Header> getNewHeaders(boolean requiresAuth)
	{
		List<Header> headerList = new ArrayList<Header>();
		if(requiresAuth && (authMode == AuthMode.Header || authMode == AuthMode.Both))
			headerList.add(new BasicHeader("Authorization", token.optString(JSON_KEY_TOKEN_TYPE) + " " + token.optString(JSON_KEY_ACCESS_TOKEN)));
		return headerList;
	}
	
	protected ResponseHandler getWithJSONResponse(String relativeUrl, List<Header> headers, RequestParams params)
	{
		ResponseHandler response = new ResponseHandler();
		// Blocking!:
		client.get(context, getAbsoluteUrl(relativeUrl), toArray(headers), params, response);
		return response;
	}

	protected ResponseHandler postWithJSONResponse(String relativeUrl, List<Header> headers, RequestParams params)
	{
		ResponseHandler response = new ResponseHandler();
		// Blocking!:
		client.post(context, getAbsoluteUrl(relativeUrl), toArray(headers), params, null /*contentType*/, response);
		return response;
	}

	private String getAbsoluteUrl(String relativeUrl)
	{
		return account.getUrl() + relativeUrl;
	}
	
	private Header[] toArray(List<Header> headers)
	{
		if(headers == null)
			return null;
		return headers.toArray(new Header[headers.size()]);
	}
	
}