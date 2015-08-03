package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.DetailsTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.ExportTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.MainTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.TransmissionTabFragment;
import android.content.Context;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.util.Log;

/**
 * @author Julia, mstevens
 *
 */
public class ProjectManagerPagerAdapter extends FragmentPagerAdapter
{
	
	static private final List<Class<? extends ProjectManagerTabFragment>> TAB_FRAGMENT_CLASSES;
	static
	{
		List<Class<? extends ProjectManagerTabFragment>> tabClasses = new ArrayList<Class<? extends ProjectManagerTabFragment>>();
		tabClasses.add(MainTabFragment.class);
		tabClasses.add(ExportTabFragment.class);
		tabClasses.add(TransmissionTabFragment.class);
		tabClasses.add(DetailsTabFragment.class);
		// add more here...
		TAB_FRAGMENT_CLASSES = Collections.unmodifiableList(tabClasses);
	}
	
	private final ProjectManagerTabFragment[] tabs = new ProjectManagerTabFragment[TAB_FRAGMENT_CLASSES.size()];
	private final Context context;
	
	public ProjectManagerPagerAdapter(Context context, FragmentManager fm)
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

}
