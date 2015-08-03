package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class MainTabFragment extends ProjectManagerTabFragment implements OnClickListener
{

	public static MainTabFragment newInstance()
	{
		MainTabFragment f = new MainTabFragment();
		return f;
	}
	
	@Override
	protected int getLayoutID()
	{
		return R.layout.tab_main;
	}

	@Override
	protected void setupUI(View rootLayout)
	{
		((Button) rootLayout.findViewById(R.id.addShortcut)).setOnClickListener(this);
		((Button) rootLayout.findViewById(R.id.removeShortcut)).setOnClickListener(this);
		//	Set shortcut icon:
		ProjectManagerActivity activity = getOwner();
		Project project = activity != null ? activity.getCurrentProject(false) : null;
		if(project != null)
			((ImageView) rootLayout.findViewById(R.id.imgShortcut)).setImageDrawable(ProjectRunHelpers.getShortcutDrawable(activity, activity.getCollectorApp().getFileStorageProvider(), project));
		
		/*// Shortcut switch:
		Switch shortcutSwitch = (Switch) ((ViewGroup) rootLayout).findViewById(R.id.btn_shortcut);
		shortcutSwitch.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener()
		{
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked)
			{
				if(activity == null)
					return;
				if(isChecked)
					activity.createShortcut();
				else
					activity.removeShortcut();
			}
		 });*/
	}

	@Override
	public void onClick(View v)
	{
		if(getOwner() != null)
			switch(v.getId())
			{
				case R.id.addShortcut:
					getOwner().createShortcut();
					break;
				case R.id.removeShortcut:
					getOwner().removeShortcut();
					break;
			}
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_main);
	}

}
