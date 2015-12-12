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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey;

import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;

/**
 * @author mstevens
 *
 */
public class GeoKeyAccount extends Correspondent
{

	private static final char ADDRESS_SEPARATOR = '|';
	private static final char ADDRESS_SEPARATOR_REPLACEMENT = ':';
	private static final char ADDRESS_SEPARATOR_PREFIX = '~';
	
	private final String url;
	private final String username;
	private final String password;
	private String token;
	
	/**
	 * @param name
	 * @param url
	 * @param username
	 * @param password
	 */
	public GeoKeyAccount(String name, String url, String username, String password)
	{
		super(name, Type.GeoKey);
		if(url == null || url.isEmpty())
			throw new IllegalArgumentException("Url cannot be null or empty!");
		this.url = url + (url.endsWith("/") ? "" : "/"); // add trailing slash
		if(username == null || username.isEmpty())
			throw new IllegalArgumentException("Username cannot be null or empty!");
		this.username = username;
		if(password == null)
			throw new IllegalArgumentException("Password cannot be null!");
		this.password = password;
	}
	
	/**
	 * To be called upon database retrieval only.
	 * 
	 * @param name
	 * @param address
	 */
	public GeoKeyAccount(String name, String address)
	{
		super(name, Type.GeoKey);
		String[] parts = address.split("\\" + ADDRESS_SEPARATOR);
		this.url = StringUtils.deescape(parts[0], ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX);
		this.username = StringUtils.deescape(parts[1], ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX);
		this.password = StringUtils.deescape(parts[2], ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX);
		if(parts.length > 3)
			this.token = StringUtils.deescape(parts[3], ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#getAddress()
	 */
	@Override
	public String getAddress()
	{
		return	StringUtils.escape(url, ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX) +
				"|" + StringUtils.escape(username, ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX) +
				"|" + StringUtils.escape(password, ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX) +
				(token != null ? "|" + StringUtils.escape(token, ADDRESS_SEPARATOR, ADDRESS_SEPARATOR_REPLACEMENT, ADDRESS_SEPARATOR_PREFIX) : "");
	}
	
	/**
	 * @return the url
	 */
	public String getUrl()
	{
		return url;
	}

	/**
	 * @return the username
	 */
	public String getUsername()
	{
		return username;
	}

	/**
	 * @return the password
	 */
	public String getPassword()
	{
		return password;
	}
	
	/**
	 * @return
	 */
	public boolean hasToken()
	{
		return token != null;
	}

	/**
	 * @return the token
	 */
	public String getToken()
	{
		return token;
	}

	/**
	 * @param token the token to set
	 */
	public void setToken(String token)
	{
		if(token == "")
			token = null;
		this.token = token;
	}

	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
	@Override
	public String toString()
	{
		return getName() + " [" + username + "@" + url + "]";
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof GeoKeyAccount)
		{
			GeoKeyAccount that = (GeoKeyAccount) obj;
			return	super.equals(that) && // Correspondent#equals(Object)
					this.url.equals(that.url) &&
					this.username.equals(that.username) &&
					this.password.equals(that.password);
			// ignore token
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + url.hashCode();
		hash = 31 * hash + username.hashCode();
		hash = 31 * hash + password.hashCode();
		// ignore token
		return hash;
	}

}
