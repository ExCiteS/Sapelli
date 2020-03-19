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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Bundle;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentTransaction;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.util.android.ViewHelpers;

/**
 * This class makes if possible to safely tie fragments to a specific Activity (i.e. ProjectManagerActivity),
 * instead of using just casting getActivity(), which feels more than a bit hackish.
 * 
 * @author mstevens
 * @see http://stackoverflow.com/a/24844574/1084488
 */
public abstract class ProjectManagerFragment extends DialogFragment
{
	
	static protected final float DIALOG_VIEW_TOP_PADDING_DP = 5.0f;

	private ProjectManagerActivity activity;
	private boolean uiReady = false;

	@Override
	public void onAttach(Activity activity)
	{
		// make sure there is no cast exception:
		this.activity = ProjectManagerActivity.class.isAssignableFrom(activity.getClass()) ? (ProjectManagerActivity) activity : null;

		super.onAttach(activity);
		
		onOwnerAttached(this.activity);
	}
	
	protected void onOwnerAttached(ProjectManagerActivity owner)
	{
		// does nothing by default
	}

	public ProjectManagerActivity getOwner()
	{
		return activity;
	}
	
	/**
	 * Note: it's OK to return null from here
	 * 
	 * @see Fragment#onCreateView(android.view.LayoutInflater, android.view.ViewGroup, android.os.Bundle)
	 */
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		if(getShowsDialog() || getLayoutID() == null)
			return super.onCreateView(inflater, container, savedInstanceState); // avoids crash (see stackoverflow link above)
		else
			return getRootLayout(inflater, container);
	}
	
	protected abstract Integer getLayoutID();
	
	protected final View getRootLayout()
	{
		return getRootLayout(getActivity().getLayoutInflater(), null);
	}
	
	protected final View getRootLayout(LayoutInflater inflater, ViewGroup container)
	{
		View rootLayout = null;
		try
		{
			// Inflate & check rootLayout:
			rootLayout = inflater.inflate(getLayoutID(), container, false);
			if(rootLayout == null)
				throw new NullPointerException("rootLayout null");
			
			// Setup UI:
			setupUI(rootLayout);
			
			// Done:
			uiReady = true;
		}
		catch(Exception e)
		{
			Log.e(getClass().getSimpleName(), "Error upon inflating/setting-up framgement UI", e);
			uiReady = false;
		}
		return rootLayout;
	}
	
	/**
	 * To be overridden by subclasses that don't need the owner activity to set-up the UI.
	 * 
	 * @param rootLayout - never null
	 * @throws Exception
	 */
	protected void setupUI(final View rootLayout) throws Exception
	{
		// Make sure we know the owner:
		final ProjectManagerActivity owner = getOwner();
		if(owner == null) // just in case...
			throw new NullPointerException("Cannot get owner activity");
		
		// Do actual UI set-up:
		setupUI(owner, rootLayout);
	}
	
	/**
	 * To be overridden by subclasses that need the owner activity to set-up the UI.
	 * 
	 * @param owner - never null
	 * @param rootLayout - never null
	 * @throws Exception
	 */
	protected void setupUI(final ProjectManagerActivity owner, final View rootLayout) throws Exception
	{
		// does nothing by default
	}
	
	/**
	 * @return the uiReady
	 */
	public final boolean isUIReady()
	{
		return uiReady && activity != null;
	}
	
	protected View setDialogView(AlertDialog dialog)
	{
		return doSetDialogView(dialog, null, null, null, null);
	}
	
	protected View setDialogView(AlertDialog dialog, int viewSpacingLeft, int viewSpacingTop, int viewSpacingRight, int viewSpacingBottom)
	{
		return doSetDialogView(dialog, viewSpacingLeft, viewSpacingTop, viewSpacingRight, viewSpacingBottom);
	}
	
	private View doSetDialogView(AlertDialog dialog, Integer viewSpacingLeft, Integer viewSpacingTop, Integer viewSpacingRight, Integer viewSpacingBottom)
	{
		View rootLayout = null;
		if(dialog != null && (rootLayout = getRootLayout()) != null)
		{
			if(viewSpacingLeft == null)
				dialog.setView(rootLayout);
			else
				dialog.setView(rootLayout, viewSpacingLeft, viewSpacingTop, viewSpacingRight, viewSpacingBottom);
		}
		return rootLayout;
	}

	@Override
	public void onDetach()
	{
		activity = null;
		super.onDetach();
	}
	
	/**
	 * Adds a child fragment to the given container view.
	 * 
	 * Note:
	 * 	We use {@link FragmentTransaction#commitAllowingStateLoss()}
	 * 	instead of {@link FragmentTransaction#commit()} to avoid the
	 * 	"Can not perform this action after onSaveInstanceState" IllegalStateException, which
	 * 	may occur if this method called while the activity is finishing (for example when it
	 * 	is being called from the onPostExecute() method of an AsyncTask which was running
	 * 	while the activity is stopped. We've seen a number of such crashes (links below).
	 * 	It should be no problem to use commitAllowingStateLoss() because (for now) the
	 * 	{@link ProjectManagerActivity} does need any state to be saved.
	 * 
	 * @param containerViewId
	 * @param child the child Fragment to add
	 * @return the child itself
	 * @see http://www.androiddesignpatterns.com/2013/08/fragment-transaction-commit-state-loss.html
	 * @see https://github.com/ExCiteS/Sapelli/issues/38
	 */
	protected <F extends Fragment> F addChild(int containerViewId, F child)
	{
		String tag = this.getClass().getSimpleName() + '|' + getTag() + '|' + child.getClass().getSimpleName();
		getFragmentManager().beginTransaction().add(containerViewId, child, tag).commitAllowingStateLoss();
		return child;
	}
	
	protected int getDialogLeftRightPaddingPx()
	{
		return ViewHelpers.getDefaultDialogPaddingPx(getActivity());
	}
	
	protected int getDialogMessageToViewSpacingPx()
	{
		return ScreenMetrics.ConvertDipToPx(getActivity(), DIALOG_VIEW_TOP_PADDING_DP);
	}

//	/**
//	 * Call to avoid duplicate id exception before the fragment (or rather the <fragment> XML that loads it)
//	 * is inflated a second time within the lifetime of an activity.
//	 * 
//	 * Note that this code is problematic...
//	 * 	We initially used this here: https://github.com/ExCiteS/Sapelli/blob/5411155/CollectorAndroid/src/uk/ac/ucl/excites/sapelli/collector/fragments/ExportFormatFragment.java#L143
//	 * 	But it caused crashes:
//	 * 	 - https://github.com/ExCiteS/Sapelli/issues/44
//	 * 	 - https://fabric.io/ucl-excites/android/apps/uk.ac.ucl.excites.sapelli.collector/issues/5591606ef505b5ccf04b458e
//	 * 	We've also had this similar issue:
//	 * 	 - https://github.com/ExCiteS/Sapelli/issues/38 (see {@link #addChild(int, Fragment)} above)
//	 * 	We won't use this anymore but we leave the code here as it could be interesting for future reference.
//	 */
//	public void forget()
//	{
//		Fragment fragment = getFragmentManager().findFragmentById(getId());
//		if(fragment != null)
//			getFragmentManager().beginTransaction().remove(fragment).commit();
//	}
	
}
