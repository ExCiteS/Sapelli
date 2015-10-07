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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.DetailsTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.ExportTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.MainTabFragment;
import android.content.Context;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.util.Log;

/**
 * @author Julia, mstevens
 */
public class ProjectManagerTabFragmentPagerAdapter extends FragmentPagerAdapter
{
	
	// STATIC -------------------------------------------------------
	static private final List<Class<? extends ProjectManagerTabFragment>> TAB_FRAGMENT_CLASSES;
	static
	{
		List<Class<? extends ProjectManagerTabFragment>> tabClasses = new ArrayList<Class<? extends ProjectManagerTabFragment>>();
		tabClasses.add(MainTabFragment.class);
		tabClasses.add(ExportTabFragment.class);
		//tabClasses.add(TransmissionTabFragment.class);
		tabClasses.add(DetailsTabFragment.class);
		// add more here...
		TAB_FRAGMENT_CLASSES = Collections.unmodifiableList(tabClasses);
	}
	
	static public int getTabIndex(Class<? extends ProjectManagerTabFragment> tabClass)
	{
		return TAB_FRAGMENT_CLASSES.indexOf(tabClass);
	}
	
	// DYNAMIC ------------------------------------------------------
	private final ProjectManagerTabFragment[] tabs = new ProjectManagerTabFragment[TAB_FRAGMENT_CLASSES.size()];
	private final Context context;
	
	public ProjectManagerTabFragmentPagerAdapter(Context context, FragmentManager fm)
	{
		super(fm);
		this.context = context;
	}

	@Override
	public CharSequence getPageTitle(int position)
	{
		return getItem(position).getTabTitle(context);
	}

	@Override
	public int getCount()
	{
		return tabs.length;
	}

	@Override
	public ProjectManagerTabFragment getItem(int position)
	{
		if(tabs[position] == null)
		{
			try
			{
				tabs[position] = TAB_FRAGMENT_CLASSES.get(position).newInstance();
			}
			catch(Exception e)
			{
				Log.e(getClass().getSimpleName(), "Cannot instantiate " + (position >= 0 && position < tabs.length && TAB_FRAGMENT_CLASSES.get(position) != null ? TAB_FRAGMENT_CLASSES.get(position).getSimpleName() : ProjectManagerTabFragment.class.getSimpleName()) + " for tab at position " + position, e);
			}
		}
		return tabs[position];
	}
	
	public void refresh()
	{
		for(int t = 0; t < getCount(); t++)
			if(tabs[t] != null)
				tabs[t].refresh();
	}

}
