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

package uk.ac.ucl.excites.sapelli.collector;

import java.io.IOException;
import java.util.zip.ZipFile;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;

/**
 * Loads build information for Resources, PackageInfo & ApplicationInfo.
 * Requires the res/values/buildinfo.xml to have been generated at compile time.
 * 
 * @author mstevens
 */			
public class BuildInfo
{

	// STATICS ------------------------------------------------------
	// Singleton:
	private static BuildInfo INSTANCE = null;
	
	public static BuildInfo GetInstance(Context context)
	{
		if(INSTANCE == null)
			INSTANCE = new BuildInfo(context);
		return INSTANCE;
	}
	
	/**
	 * @return the previously instantiated BuildInfo instance, or null if there is none 
	 */
	public static BuildInfo GetInstance()
	{
		return INSTANCE;
	}
	
	// DYNAMICS -----------------------------------------------------
	private ApplicationInfo ai;
	private PackageInfo pi;
	private Resources res;
	
	private BuildInfo(Context ctx)
	{
		try
		{
			this.ai = ctx.getPackageManager().getApplicationInfo(ctx.getPackageName(), 0);
			this.pi = ctx.getPackageManager().getPackageInfo(ctx.getPackageName(), 0);
		}
		catch(NameNotFoundException ignore) { }
		this.res = ctx.getResources();
	}
	
	public String getUsername()
	{
		return "";
	}
	
	public DateTime getTimeStamp()
	{
		ZipFile zf = null;
		try
		{
			zf = new ZipFile(ai.sourceDir);			
			return new DateTime(zf.getEntry("classes.dex").getTime());
		}
		catch(Exception e)
		{
			return DateTime.now();
		}
		finally
		{
			if(zf != null)
				try
				{
					zf.close();
				}
				catch(IOException ignore) {}
		}
	}
	
	public String getTimeStampString()
	{
		DateTime ts = getTimeStamp();
		return TimeUtils.ISOWithoutMSFormatter.withZone(ts.getZone()).print(ts);
	}
	
	public String getBranch()
	{
		return "";
	}
	
	public String getLastCommitHash()
	{
		return "";
	}
	
	public String getLastCommitTag()
	{
		return "";
	}
	
	public boolean isChangesSinceLastCommit()
	{
		return false;
	}
	
	public boolean isDemoBuild()
	{
		return false;
	}
	
	public String getNameAndVersion()
	{
		TransactionalStringBuilder bldr = new TransactionalStringBuilder(" ");
		bldr.append(res.getString(R.string.app_name));
		if(pi != null)
			bldr.append("v" + pi.versionName);
		else
			bldr.append("[version unknown]");
		return bldr.toString();
	}
	
	public String getExtraVersionInfo()
	{
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("; ");
		if(pi != null)
		{
			bldr.append("versionCode: " + pi.versionCode);
			bldr.append(BuildConfig.DEBUG ? "debug" : "release");
			if(isDemoBuild())
				bldr.append("demo");
		}
		return bldr.toString();
	}
	
	public String getBuildInfo()
	{
		return	"Built on " + getTimeStampString() + " by " + getUsername()  + " using " + getBranch() + " branch, revision " + getLastCommitHash() + " with" + (isChangesSinceLastCommit() ? "" : "out") + " changes";
	}
	
	public String getAllInfo()
	{
		return getNameAndVersion() + "\n[" + getExtraVersionInfo() + "; " + getBuildInfo() + "]";
	}
			
}