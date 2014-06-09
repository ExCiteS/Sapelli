package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.view.ViewGroup;

public class PagerAdapter extends FragmentPagerAdapter {

	private List<Project> projects = new ArrayList<Project>();
	
	
	/**
	 * @return the projects
	 */
	public List<Project> getProjects() {
		return projects;
	}

	public PagerAdapter(FragmentManager fm, List<Project> projects) {
		super(fm);
		this.projects = projects;
	}

	@Override
	public CharSequence getPageTitle(int position) {

		return projects.get(position).getName();
	}

	@Override
	public int getCount() {
		return projects.size();
	}

	@Override
	public Fragment getItem(int position) {
		return ProjectFragment.newInstance(position);
	}
	

	
	

}
