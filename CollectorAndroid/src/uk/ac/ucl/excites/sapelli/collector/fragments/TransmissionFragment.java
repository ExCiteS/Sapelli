package uk.ac.ucl.excites.sapelli.collector.fragments;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
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

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia
 */
public class TransmissionFragment extends Fragment implements OnClickListener {

	private static final String PAGE_POSITION = "position";

	public static TransmissionFragment newInstance(int position) {
		TransmissionFragment f = new TransmissionFragment();
		Bundle b = new Bundle();
		b.putInt(PAGE_POSITION, position);
		f.setArguments(b);
		return f;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		LinearLayout rootLayout = (LinearLayout) inflater.inflate(R.layout.fragment_transmission, container, false);

		return rootLayout;

	}

	@Override
	public void onClick(View arg0) {
		// TODO Auto-generated method stub

	}

}
