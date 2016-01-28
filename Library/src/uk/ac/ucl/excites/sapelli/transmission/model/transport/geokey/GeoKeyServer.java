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
public class GeoKeyServer extends Correspondent
{
	
	// STATIC -----------------------------------------------------------------
	static public final String[] ALLOWED_URL_SCHEMES = {"http", "https"};
	
	static public final UrlValidator URL_VALIDATOR = new UrlValidator(ALLOWED_URL_SCHEMES, UrlValidator.ALLOW_LOCAL_URLS | UrlValidator.NO_FRAGMENTS);
	
	static public final EmailValidator EMAIL_VALIDATOR = EmailValidator.getInstance();
	
	static public final String checkName(String name, String url)
	{
		// Check name, use url as fallback:
		if(name == null || name.isEmpty())
			return URLUtils.stripTrailingSlash(URLUtils.stripHTTP(url)); // get rid of "http[s]://" and trailing slash
		else
			return name;
	}
	
	/**
	 * Called to create a new GeoKeyAccount.
	 * 
	 * @param name
	 * @param url
	 * @param userEmail - may be null or empty
	 * @param userPassword - may be null or empty if userEmail is too
	 * @return
	 */
	static public GeoKeyServer CreateNew(String name, String url, String userEmail, String userPassword)
	{		
		// Check url:
		if(url == null || url.isEmpty())
			throw new IllegalArgumentException("Url cannot be null or empty!");
		// add trailing slash:
		url = URLUtils.addTrailingSlash(url);
		if(!URL_VALIDATOR.isValid(url))
			throw new IllegalArgumentException("Url is invalid: " + url);
		
		// Check email:
		if("".equals(userEmail))
			userEmail = null;
		if(userEmail != null && !EMAIL_VALIDATOR.isValid(userEmail))
			throw new IllegalArgumentException("E-mail address is invalid: " + userEmail);
		
		// Check password:
		if("".equals(userPassword))
			userPassword = null;
		if(userPassword == null && userEmail != null)
			throw new IllegalArgumentException("Password is manadatory if email is provided");
		
		// Check name, use url as fallback:
		name = checkName(name, url);

		return new GeoKeyServer(name, url, userEmail, userPassword);
	}
	
	static private final ColumnSet ADDRESS_COLUMNS = new ColumnSet("Address", false);
	static private final StringColumn ADDRESS_COLUMN_URL = ADDRESS_COLUMNS.addColumn(new StringColumn("URL", false));
	static private final StringColumn ADDRESS_COLUMN_USER_EMAIL = ADDRESS_COLUMNS.addColumn(new StringColumn("UserEmail", true));
	static private final StringColumn ADDRESS_COLUMN_USER_PASSWORD = ADDRESS_COLUMNS.addColumn(new StringColumn("UserPassword", true));
	static private final StringColumn ADDRESS_COLUMN_USER_TOKEN = ADDRESS_COLUMNS.addColumn(new StringColumn("UserToken", true));
	static private final StringColumn ADDRESS_COLUMN_USER_DISPLAY_NAME = ADDRESS_COLUMNS.addColumn(new StringColumn("UserDisplayName", true), true);
	
	static public final String ANONYMOUS_USER = "Anonymous";
	
	// DYNAMIC ----------------------------------------------------------------
	private final String url;
	private String userEmail;
	private String userPassword;
	private String userDisplayName;
	private String userToken;
	
	/**
	 * Called to create a new GeoKeyAccount.
	 * 
	 * @param name
	 * @param url
	 * @param userEmail
	 * @param userPassword
	 */
	private GeoKeyServer(String name, String url, String userEmail, String userPassword)
	{
		super(null, name, Type.GeoKey);
		this.url = url;
		this.userEmail = userEmail;
		this.userPassword = userPassword;
	}
	
	/**
	 * To be called upon database retrieval only.
	 * 
	 * @param localID
	 * @param name
	 * @param address
	 */
	public GeoKeyServer(int localID, String name, String address)
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
		this.userEmail = ADDRESS_COLUMN_USER_EMAIL.retrieveValue(addressValues);
		this.userPassword = ADDRESS_COLUMN_USER_PASSWORD.retrieveValue(addressValues);
		this.userToken = ADDRESS_COLUMN_USER_TOKEN.retrieveValue(addressValues);
		this.userDisplayName = ADDRESS_COLUMN_USER_DISPLAY_NAME.retrieveValue(addressValues);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#getAddress()
	 */
	@Override
	public String getAddress()
	{
		return new ValueSet<ColumnSet>(ADDRESS_COLUMNS, url, userEmail, userPassword, userToken, userDisplayName).serialise();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Correspondent#setName(java.lang.String)
	 */
	@Override
	public void setName(String name)
	{
		super.setName(checkName(name, url));
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
	public String getUserEmail()
	{
		return userEmail;
	}

	/**
	 * @param userEmail the e-mail address to set
	 */
	public void setUserCredentials(String newUserEmail, String newUserPassword)
	{
		if(!Objects.equals(this.userEmail, newUserEmail) || !Objects.equals(this.userPassword, newUserPassword))
		{
			this.userEmail = newUserEmail;
			this.userPassword = newUserPassword;
			// Wipe display name & token:
			this.userDisplayName = null;
			this.userToken = null;
		}
	}

	/**
	 * @return the password
	 */
	public String getUserPassword()
	{
		return userPassword;
	}
	
	/**
	 * @return whether or not we have user credentials (i.e. email & password)
	 */
	public boolean hasUserCredentials()
	{
		return userEmail != null && userPassword != null;
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
	public boolean hasUserToken()
	{
		return hasUserCredentials() && userToken != null;
	}

	/**
	 * @return the token
	 */
	public String getUserToken()
	{
		return userToken;
	}

	/**
	 * @param token the token to set
	 */
	public void setToken(String token)
	{
		if("".equals(token))
			token = null;
		this.userToken = token;
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
		return getName() + " [" + (hasUserCredentials() ? (hasUserDisplayName() ? userDisplayName : userEmail) : ANONYMOUS_USER) + "@" + url + "]";
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true;
		if(obj instanceof GeoKeyServer)
		{
			GeoKeyServer that = (GeoKeyServer) obj;
			return	super.equals(that) && // Correspondent#equals(Object)
					this.url.equals(that.url) &&
					Objects.equals(this.userEmail, that.userEmail) &&
					Objects.equals(this.userPassword, that.userPassword) &&
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
		hash = 31 * hash + Objects.hashCode(userEmail);
		hash = 31 * hash + Objects.hashCode(userPassword);
		hash = 31 * hash + Objects.hashCode(userDisplayName);
		// ignore token
		return hash;
	}

	@Override
	public boolean canBeSwappedWithoutNewModelQuery(Correspondent another)
	{
		if(this == another)
			return true;
		if(another instanceof GeoKeyServer)
		{
			GeoKeyServer that = (GeoKeyServer) another;
			return	this.url.equals(that.url) &&
					Objects.equals(this.userEmail, that.userEmail) &&
					Objects.equals(this.userPassword, that.userPassword);
		}
		else
			return false;
	}

}
