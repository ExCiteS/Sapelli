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

import org.apache.commons.validator.routines.EmailValidator;
import org.apache.commons.validator.routines.UrlValidator;

import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.shared.util.URLUtils;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;

/**
 * @author mstevens
 *
 */
public class GeoKeyAccount extends Correspondent
{
	
	// STATIC -----------------------------------------------------------------
	static public final String[] ALLOWED_URL_SCHEMES = {"http", "https"};
	
	static public final UrlValidator URL_VALIDATOR = new UrlValidator(ALLOWED_URL_SCHEMES, UrlValidator.ALLOW_LOCAL_URLS | UrlValidator.NO_FRAGMENTS);
	
	static public final EmailValidator EMAIL_VALIDATOR = EmailValidator.getInstance();
	
	/**
	 * Called to create a new GeoKeyAccount.
	 * 
	 * @param name
	 * @param url
	 * @param email
	 * @param password
	 * @return
	 */
	static public GeoKeyAccount CreateNew(String name, String url, String email, String password)
	{		
		// Check url:
		if(url == null || url.isEmpty())
			throw new IllegalArgumentException("Url cannot be null or empty!");
		// add trailing slash:
		url = URLUtils.addTrailingSlash(url);
		if(!URL_VALIDATOR.isValid(url))
			throw new IllegalArgumentException("Url is invalid: " + url);
		
		// TODO make user credentials optional?
		if(email == null || email.isEmpty())
			throw new IllegalArgumentException("Username cannot be null or empty!");
		if(!EMAIL_VALIDATOR.isValid(email))
			throw new IllegalArgumentException("E-mail address is invalid: " + email);
		if(password == null)
			throw new IllegalArgumentException("Password cannot be null!");
		
		// Check name, use url as fallback:
		if(name == null || name.isEmpty())
			name = URLUtils.stripTrailingSlash(URLUtils.stripHTTP(url)); // get rid of "http[s]://" and trailing slash

		return new GeoKeyAccount(
			name,
			url,
			email,
			password);
	}
	
	static private final ColumnSet ADDRESS_COLUMNS = new ColumnSet("Address", false);
	static private final StringColumn ADDRESS_COLUMN_URL = ADDRESS_COLUMNS.addColumn(new StringColumn("URL", false));
	static private final StringColumn ADDRESS_COLUMN_EMAIL = ADDRESS_COLUMNS.addColumn(new StringColumn("Email", true));
	static private final StringColumn ADDRESS_COLUMN_PASSWORD = ADDRESS_COLUMNS.addColumn(new StringColumn("Password", true));
	static private final StringColumn ADDRESS_COLUMN_TOKEN = ADDRESS_COLUMNS.addColumn(new StringColumn("Token", true));
	static private final StringColumn ADDRESS_COLUMN_USER_DISPLAY_NAME = ADDRESS_COLUMNS.addColumn(new StringColumn("UserDisplayName", true), true);
	
	// DYNAMIC ----------------------------------------------------------------
	private final String url;
	private String email;
	private String password;
	private String userDisplayName;
	private String token;
	
	/**
	 * Called to create a new GeoKeyAccount.
	 * 
	 * @param name
	 * @param url
	 * @param email
	 * @param password
	 */
	private GeoKeyAccount(String name, String url, String email, String password)
	{
		super(null, name, Type.GeoKey);
		this.url = url;
		this.email = email;
		this.password = password;
	}
	
	/**
	 * To be called upon database retrieval only.
	 * 
	 * @param localID
	 * @param name
	 * @param address
	 */
	public GeoKeyAccount(int localID, String name, String address)
	{
		super(localID, name, Type.GeoKey);
		// Parse address string:
		ValueSet<ColumnSet> addressValues;
		try
		{
			addressValues = new ValueSet<ColumnSet>(ADDRESS_COLUMNS, address);
		}
		catch(Exception e)
		{	// should never happen
			e.printStackTrace();
			this.url = null;
			return;
		}
		this.url = ADDRESS_COLUMN_URL.retrieveValue(addressValues);
		this.email = ADDRESS_COLUMN_EMAIL.retrieveValue(addressValues);
		this.password = ADDRESS_COLUMN_PASSWORD.retrieveValue(addressValues);
		this.token = ADDRESS_COLUMN_TOKEN.retrieveValue(addressValues);
		this.userDisplayName = ADDRESS_COLUMN_USER_DISPLAY_NAME.retrieveValue(addressValues);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#getAddress()
	 */
	@Override
	public String getAddress()
	{
		return new ValueSet<ColumnSet>(ADDRESS_COLUMNS, url, email, password, token, userDisplayName).serialise();
	}
	
	/**
	 * @return the url
	 */
	public String getUrl()
	{
		return url;
	}

	/**
	 * @return the email
	 */
	public String getEmail()
	{
		return email;
	}

	/**
	 * @param email the email to set
	 */
	public void setEmail(String email)
	{
		this.email = email;
	}

	/**
	 * @return the password
	 */
	public String getPassword()
	{
		return password;
	}
	
	/**
	 * @param password the password to set
	 */
	public void setPassword(String password)
	{
		this.password = password;
	}
	/**
	 * @return whether or not a userDisplayName has been set
	 */
	public boolean hasUserDisplayName()
	{
		return userDisplayName != null;
	}
	
	/**
	 * @return the userDisplayName
	 */
	public String getUserDisplayName()
	{
		return userDisplayName;
	}

	/**
	 * @param userDisplayName the userDisplayName to set
	 */
	public void setUserDisplayName(String userDisplayName)
	{
		if("".equals(userDisplayName))
			userDisplayName = null;
		this.userDisplayName = userDisplayName;
	}

	/**
	 * @return whether or not a token has been set
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
		if("".equals(token))
			token = null;
		this.token = token;
	}

	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#favoursLosslessPayload()
	 */
	@Override
	public boolean favoursLosslessPayload()
	{
		return true;
	}

	@Override
	public String toString()
	{
		return getName() + " [" + email + "@" + url + "]";
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
					this.email.equals(that.email) &&
					this.password.equals(that.password) &&
					Objects.equals(this.userDisplayName, that.userDisplayName);
			// ignore token
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + url.hashCode();
		hash = 31 * hash + email.hashCode();
		hash = 31 * hash + password.hashCode();
		hash = 31 * hash + Objects.hashCode(userDisplayName);
		// ignore token
		return hash;
	}

}
