/**
 * Sapelli data collection platform: http://sapelli.org
 * <p/>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p/>
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * <p/>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p/>
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.res.Resources;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import hu.supercluster.paperwork.Paperwork;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;

/**
 * Loads build information for Resources, PackageInfo & ApplicationInfo.
 * <p/>
 *
 * @author mstevens, Michalis Vitos
 */
public class BuildInfo
{

	// STATICS ------------------------------------------------------
	// Singleton:
	private static BuildInfo INSTANCE = null;

	public static BuildInfo GetInstance(Context context)
	{
		if (INSTANCE == null)
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
	private PackageInfo pi;
	private Resources res;
	private Paperwork paperwork;

	private BuildInfo(Context ctx)
	{
		try
		{
			this.pi = ctx.getPackageManager().getPackageInfo(ctx.getPackageName(), 0);
		}
		catch (PackageManager.NameNotFoundException ignore) { }
		this.res = ctx.getResources();
		paperwork = new Paperwork(ctx);
	}

	public String getUsername()
	{
		return paperwork.get("userName");
	}

	public DateTime getTimeStamp()
	{
		return new DateTime(paperwork.get("buildTimeUTC"), DateTimeZone.UTC);
	}

	public String getTimeStampString()
	{
		DateTime ts = getTimeStamp();
		return TimeUtils.ISOWithoutMSFormatter.withZone(ts.getZone()).print(ts);
	}

	public String getBranch()
	{
		return paperwork.get("gitBranch");
	}

	public String getLastCommitHash()
	{
		return paperwork.get("gitSha");
	}

	public String getLastCommitTag()
	{
		return paperwork.get("gitTag");
	}

	public boolean isChangesSinceLastCommit()
	{
		final String info = paperwork.get("gitInfo");
		return info.substring(info.lastIndexOf('-') + 1).contentEquals("dirty");
	}

	public boolean isDemoBuild()
	{
		return false;
	}

	public String getNameAndVersion()
	{
		TransactionalStringBuilder bldr = new TransactionalStringBuilder(" ");
		bldr.append(res.getString(R.string.app_name));
		if (pi != null)
			bldr.append("v" + pi.versionName);
		else
			bldr.append("[version unknown]");
		return bldr.toString();
	}

	public String getExtraVersionInfo()
	{
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("; ");
		if (pi != null)
		{
			bldr.append("versionCode: " + pi.versionCode);
			bldr.append(BuildConfig.DEBUG ? "debug" : "release");
			if (isDemoBuild())
				bldr.append("demo");
		}
		return bldr.toString();
	}

	public String getBuildInfo()
	{
		return String.format("Built on %s by %s using %s branch, revision %s with%s changes",
		                     getTimeStampString(),
		                     getUsername(),
		                     getBranch(),
		                     getLastCommitHash(),
		                     (isChangesSinceLastCommit() ? "" : "out"));
	}

	public String getAllInfo()
	{
		return getNameAndVersion() + "\n[" + getExtraVersionInfo() + "; " + getBuildInfo() + "]";
	}
}