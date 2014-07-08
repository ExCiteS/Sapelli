package uk.ac.ucl.excites.sapelli.collector.fragments;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.ui.Switch;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia
 */
public class ProjectFragment extends Fragment implements OnClickListener {

	public static ProjectFragment newInstance() {
		ProjectFragment f = new ProjectFragment();
		return f;
	}

	// Layouts
	private RelativeLayout rootLayout;
	private Button addShortcut;
	private Button removeShortcut;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		rootLayout = (RelativeLayout) inflater.inflate(R.layout.fragment_project, container, false);
		addShortcut = (Button) ((ViewGroup) rootLayout).findViewById(R.id.addShortcut);
		addShortcut.setOnClickListener(this);
		removeShortcut = (Button) ((ViewGroup) rootLayout).findViewById(R.id.removeShortcut);
		removeShortcut.setOnClickListener(this);

		// Shortcut
		//		Switch shortcutSwitch = (Switch) ((ViewGroup) rootLayout).findViewById(R.id.btn_shortcut);
		//		shortcutSwitch.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
		//
		//			@Override
		//			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		//				if (isChecked)
		//
		//					((ProjectManagerActivity) getActivity()).createShortcut();
		//				else
		//					((ProjectManagerActivity) getActivity()).removeShortcut();
		//			}
		//		});

		return rootLayout;

	}

	@Override
	public void onClick(View v) {

		switch (v.getId()) {
		case R.id.addShortcut:
			((ProjectManagerActivity) getActivity()).createShortcut();
			break;
		case R.id.removeShortcut:
			((ProjectManagerActivity) getActivity()).removeShortcut();
			break;

		}

	}

}
