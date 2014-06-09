package uk.ac.ucl.excites.sapelli.collector.fragments;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.ui.Switch;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia
 */
public class ProjectFragment extends ExportFragment implements OnClickListener {

	private static final String PAGE_POSITION = "position";

	public static ProjectFragment newInstance(int position) {
		ProjectFragment f = new ProjectFragment();
		Bundle b = new Bundle();
		b.putInt(PAGE_POSITION, position);
		f.setArguments(b);
		return f;
	}

	// Layouts
	private LinearLayout transmissionHeader;
	private LinearLayout transmissionView;
	private LinearLayout exportHeader;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		RelativeLayout rootLayout = (RelativeLayout) inflater.inflate(R.layout.fragment_project, container, false);
		Button runProject = (Button) ((ViewGroup) rootLayout).findViewById(R.id.btn_runProject);
		runProject.setOnClickListener(this);

		// Shortcut
		Switch shortcutSwitch = (Switch) ((ViewGroup) rootLayout).findViewById(R.id.btn_shortcut);
		shortcutSwitch.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {

			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked)

					((ProjectManagerActivity) getActivity()).createShortcut();
				else
					((ProjectManagerActivity) getActivity()).removeShortcut();
			}
		});

		// Transmission
		transmissionHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.transmissionHeader);
		transmissionView = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.transmissionView);
		transmissionHeader.setOnClickListener(this);

		// Export 
		exportHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.exportHeader);
		final LinearLayout pageView = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.scrollableView);
		pageView.addView(exportFragment);
		exportFragment.setBackgroundResource(R.layout.drop_shadow_bottom);
		exportFragment.setVisibility(View.GONE);
		exportHeader.setOnClickListener(this);

		return rootLayout;

	}

	/**
	 * Handles layout changes when views are expanded
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void toggleView(View clickview, View expandview) {
		expandview.setVisibility(expandview.isShown() ? View.GONE : View.VISIBLE);
		clickview.setBackgroundResource(expandview.isShown() ? R.layout.drop_shadow_top : R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1)).setImageResource(expandview.isShown() ? R.drawable.ic_action_collapse : R.drawable.ic_action_expand);

	}

	@Override
	public void onClick(View v) {
		super.onClick(v);
		switch (v.getId()) {
		case R.id.btn_runProject:
			((ProjectManagerActivity) getActivity()).runProject();
			break;
		case R.id.transmissionHeader:
			toggleView(transmissionHeader, transmissionView);
			break;
		case R.id.exportHeader:
			toggleView(exportHeader, exportFragment);
			break;
		}

	}

}
