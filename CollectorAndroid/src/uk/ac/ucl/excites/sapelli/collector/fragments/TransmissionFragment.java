package uk.ac.ucl.excites.sapelli.collector.fragments;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.ui.Switch;
import android.opengl.Visibility;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
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
public class TransmissionFragment extends Fragment implements OnClickListener {

	private static final String PAGE_POSITION = "position";

	// Layouts
	private LinearLayout sendDataHeader;
	private LinearLayout sendDataView;
	private LinearLayout receiveDataHeader;
	private TextView receiveDataView;
	private CheckBox checkSend;
	private CheckBox checkReceive;
	private ImageView expandSend;
	private ImageView expandReceive;

	public static TransmissionFragment newInstance() {
		TransmissionFragment f = new TransmissionFragment();
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

		// Transmission
		sendDataHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.sendHeader);
		sendDataView = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.sendSettings);
		sendDataHeader.setOnClickListener(this);

		receiveDataHeader = (LinearLayout) ((ViewGroup) rootLayout).findViewById(R.id.receiveHeader);
		receiveDataView = (TextView) ((ViewGroup) rootLayout).findViewById(R.id.receiveSettings);
		receiveDataHeader.setOnClickListener(this);

		checkSend = (CheckBox) ((ViewGroup) rootLayout).findViewById(R.id.checkSend);
		checkSend.setOnClickListener(this);
		checkReceive = (CheckBox) ((ViewGroup) rootLayout).findViewById(R.id.checkReceive);
		checkReceive.setOnClickListener(this);

		// on old Android versions the label overlaps the checkbox
		if (android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.JELLY_BEAN_MR1) {
			addChbxPadding(checkSend);
			addChbxPadding(checkReceive);
		}

		expandSend = (ImageView) ((ViewGroup) rootLayout).findViewById(R.id.expandSend);
		expandReceive = (ImageView) ((ViewGroup) rootLayout).findViewById(R.id.expandReceive);

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

	/**
	 * Makes view non-expandable
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void disableToggle(View clickview, View expandview) {
		expandview.setVisibility(View.GONE);
		clickview.setBackgroundResource(R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1)).setVisibility(View.GONE);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.sendHeader:
			toggleView(sendDataHeader, sendDataView);
			break;
		case R.id.receiveHeader:
			toggleView(receiveDataHeader, receiveDataView);
			break;
		case R.id.checkSend:
			if (checkSend.isChecked()) {
				expandSend.setVisibility(View.VISIBLE);
				toggleView(sendDataHeader, sendDataView);
			} else {
				disableToggle(sendDataHeader, sendDataView);
			}
			break;
		case R.id.checkReceive:
			if (checkReceive.isChecked()) {
				expandReceive.setVisibility(View.VISIBLE);
				toggleView(receiveDataHeader, receiveDataView);
			} else {
				disableToggle(receiveDataHeader, receiveDataView);
			}
			break;
		}
	}

	// http://stackoverflow.com/questions/4037795/android-spacing-between-checkbox-and-text
	private void addChbxPadding(CheckBox checkbox) {
		checkbox.setPadding(checkbox.getPaddingLeft() + (int) (30.0f * this.getResources().getDisplayMetrics().density + 0.5f), checkbox.getPaddingTop(), checkbox.getPaddingRight(), checkbox.getPaddingBottom());
	}

}
