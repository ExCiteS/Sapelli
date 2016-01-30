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

package uk.ac.ucl.excites.sapelli.shared.util.android.http;

import org.json.JSONArray;
import org.json.JSONObject;

import com.loopj.android.http.JsonHttpResponseHandler;

import cz.msebera.android.httpclient.Header;

/**
 * @author mstevens
 *
 */
public class ResponseHandler extends JsonHttpResponseHandler
{

	private JSONObject responseObject;
	private JSONArray responseArray;
	private String responseString;
	private Throwable error;

	public boolean hasResponseObject()
	{
		return responseObject != null;
	}

	/**
	 * @return the responseObject
	 */
	public JSONObject getResponseObject()
	{
		return responseObject;
	}

	public boolean hasResponseArray()
	{
		return responseArray != null;
	}

	/**
	 * @return the responseArray
	 */
	public JSONArray getResponseArray()
	{
		return responseArray;
	}

	public boolean hasResponseString()
	{
		return responseString != null;
	}

	/**
	 * @return the responseString
	 */
	public String getResponseString()
	{
		return responseString;
	}

	public boolean hasError()
	{
		return error != null;
	}

	/**
	 * @return the error
	 */
	public Throwable getError()
	{
		return error;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onSuccess(int, cz.msebera.android.httpclient.Header[], org.json.JSONObject)
	 */
	@Override
	public void onSuccess(int statusCode, Header[] headers, JSONObject response)
	{
		responseObject = response;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onFailure(int, cz.msebera.android.httpclient.Header[], java.lang.Throwable, org.json.JSONObject)
	 */
	@Override
	public void onFailure(int statusCode, Header[] headers, Throwable throwable, JSONObject errorResponse)
	{
		responseObject = errorResponse;
		error = throwable;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onSuccess(int, cz.msebera.android.httpclient.Header[], org.json.JSONArray)
	 */
	@Override
	public void onSuccess(int statusCode, Header[] headers, JSONArray response)
	{
		responseArray = response;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onFailure(int, cz.msebera.android.httpclient.Header[], java.lang.Throwable, org.json.JSONArray)
	 */
	@Override
	public void onFailure(int statusCode, Header[] headers, Throwable throwable, JSONArray errorResponse)
	{
		responseArray = errorResponse;
		error = throwable;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onSuccess(int, cz.msebera.android.httpclient.Header[], java.lang.String)
	 */
	@Override
	public void onSuccess(int statusCode, Header[] headers, String responseString)
	{
		this.responseString = responseString;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.loopj.android.http.JsonHttpResponseHandler#onFailure(int, cz.msebera.android.httpclient.Header[], java.lang.String, java.lang.Throwable)
	 */
	@Override
	public void onFailure(int statusCode, Header[] headers, String responseString, Throwable throwable)
	{
		this.responseString = responseString;
		error = throwable;
	}

}
