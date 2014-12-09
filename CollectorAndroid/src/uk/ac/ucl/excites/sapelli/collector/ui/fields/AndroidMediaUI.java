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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.util.HashMap;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.ui.AndroidControlsUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.ControlsUI.Control;
import uk.ac.ucl.excites.sapelli.collector.ui.PickerView;
import uk.ac.ucl.excites.sapelli.collector.ui.animation.ClickAnimator;
import uk.ac.ucl.excites.sapelli.collector.ui.items.EmptyItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.LayeredItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * An abstract class that represents a generic interface for capturing and reviewing media, with
 * the automatic usage of a "gallery" mode for review if the media field can have multiple attachments.
 * 
 * Subclasses must implement methods that are called when various control buttons are pressed, such as "onCapture" when
 * the media capture button (e.g. camera shutter) is tapped.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public abstract class AndroidMediaUI<MF extends MediaField> extends MediaUI<MF, View, CollectorView>
{
	
	protected static enum DisplayState
	{
		CAPTURE,
		GALLERY,
		SINGLE_ITEM_REVIEW, // alternative to gallery mode when field.max = 1
		SINGLE_REVIEW_FROM_GALLERY // showing a single item from gallery
	};

	// private static final String TAG = "AndroidMediaUI";
	// keys to use when obtaining values from field arguments:
	private static final String REVIEW_FILE_PATH_KEY = "REVIEW_FILE_PATH";
	private static final String GO_TO_CAPTURE_KEY = "GO_TO_CAPTURE";

	protected CollectorController controller; // keep reference to the Android-specific controller (shadows controller in FieldUI)
	private boolean maxReached; // whether or not the maximum number of pieces of media have been captured for this field
	private boolean mediaItemsChanged = false; 	// whether or not the media items have been changed (whether the gallery needs to be redrawn)
	protected File captureFile; // file used to store media while it is being captured

	private OnPageView pageView;
	
	private CaptureView captureView;
	private GalleryView galleryView;
	
	private HashMap<File, Item> galleryCache; // hashmap used to cache gallery items so that every attachment doesn't have to be reloaded when a change is made
	
	// global variable that holds the params for the capture/discard buttons
	private LinearLayout.LayoutParams buttonParams; 

	public AndroidMediaUI(MF field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		this.controller = controller; // Android-specific controller
		maxReached = (field.getCount(controller.getCurrentRecord()) >= field.getMax());
		// create button params to be used for all bottom-of-screen buttons:
		buttonParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, ScreenMetrics.ConvertDipToPx(collectorUI.getContext(), AndroidControlsUI.CONTROL_HEIGHT_DIP));
		// add margin above button to separate it from content:
		buttonParams.setMargins(0, collectorUI.getSpacingPx(), 0, 0);
	}

	/**
	 * Returns the appropriate View that represents this field.
	 * <br>
	 * If not on a page,
	 * <ul>
	 * <li> if no media has been captured, or the capture UI has been specifically requested (e.g. when the 
	 * user wants to capture more media from the gallery), return the capture UI.</li>
	 * <li> else
	 * <ul>
	 * <li> if the field can only have one attachment and one has been made, return the single-item review UI. 
	 * Also return the single-item review UI if an item has been selected from the gallery. </li>
	 * <li> else (field max is larger than 1 and an item hasn't been selected), return the gallery UI.</li>
	 * </ul>
	 * </li>
	 * </ul>
	 */
	@Override
	protected View getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		//TODO take "enabled" into account
		if(onPage)
		{
			if(pageView == null)
				pageView = new OnPageView(collectorUI.getContext());
			
			// Enable/disable:
			pageView.setEnabled(enabled); // also sets up event listeners!
			return pageView;
		}

		// get the current display state:
		DisplayState currentState = getCurrentDisplayState();
		// Log.d(TAG,"Current state: "+currentState.name());

		// decide which UI to return and return it:
		switch(currentState)
		{
		case CAPTURE:

			captureView = new CaptureView(collectorUI.getContext(), isMaximiseCaptureButton());
			// keep a reference to the capture UI so the capture button can later be maximised/minimised
			return captureView;

		case SINGLE_ITEM_REVIEW:

			return new ReviewView(collectorUI.getContext(), field.getLastAttachment(controller.getFileStorageProvider(), record));

		case SINGLE_REVIEW_FROM_GALLERY:

			return new ReviewView(collectorUI.getContext(), new File(controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY)));

		default: // Return gallery

			if(galleryView == null)
				galleryView = new GalleryView(collectorUI.getContext());

			if(galleryCache == null || newRecord)
				// wipe the cache whenever newRecord is true since we don't want any stale items in the gallery
				galleryCache = new HashMap<File, Item>();

			// force the gallery to update its contents and its button:
			galleryView.refresh();

			return galleryView;
		}
	}

	/**
	 * @return the current display state based on the field's arguments and properties.
	 */
	private DisplayState getCurrentDisplayState()
	{
		if(field.getCount(controller.getCurrentRecord()) == 0 || controller.getCurrentFieldArguments().getBoolean(GO_TO_CAPTURE_KEY, false))
			// either have no files to review or have been explicitly told to go to capture state
			return DisplayState.CAPTURE;

		if(field.getMax() == 1)
			// not in capture and can have max. 1 attachment, so go to single item review
			return DisplayState.SINGLE_ITEM_REVIEW;

		if(controller.getCurrentFieldArguments().getValue(REVIEW_FILE_PATH_KEY) != null)
			// not in capture, max > 1, and there was a filepath in the arguments so show that file for review
			return DisplayState.SINGLE_REVIEW_FROM_GALLERY;

		// have multiple attachments, not going to capture, and no filepath for review so go to gallery
		return DisplayState.GALLERY;
	}

	/**
	 * Ensures that the media file reference is nullified after a capture is made so that it is not deleted
	 * from the {@link AndroidMediaUI#cancel()} method.
	 */
	@Override
	public void attachMedia(File mediaAttachment)
	{
		super.attachMedia(mediaAttachment);
		captureFile = null;
	}

	/**
	 * When this field is cancelled, delete any half-captured media (e.g. pressing 'back' 
	 * during video recording).
	 */
	@Override
	protected void cancel()
	{
		if (captureFile != null) {
			// last capture has been implicitly discarded.
			// Log.d(TAG, "Deleting discarded file...");
			captureFile.delete();
		}
		captureFile = null;
	}

	/**
	 * Override the normal "back" behaviour so that the user is returned to the gallery when "back" is pressed
	 * from the single-item review UI.
	 */
	@Override
	public boolean handleControlEvent(Control control)
	{
		// ignore user's request to return to capture now we are leaving the field (do this in any case)
		controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY);

		if(control.equals(Control.BACK) && getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
		{
			// remove the filepath from the field's arguments so we do not re-enter single-item review unintentionally
			controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY);
			// we are currently in single-item review from gallery, so back button must just return us to gallery
			controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			return true;
		}
		// do not change the button behaviour in any other case (other than removing the capture request)
		return false;
	}
	
	/**
	 * Force the back button to be shown if in single item review (from gallery) so that the user can always go back
	 * to the gallery (regardless of field history on the stack).
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI#isShowBack()
	 */
	@Override
	protected boolean isShowBack()
	{
		if(getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
			return true;
		return super.isShowBack();
	}

	/**
	 * Hide the forward button when reviewing a single item from the gallery.
	 *
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.fields.SelfLeavingFieldUI#isShowForward()
	 */
	@Override
	public boolean isShowForward()
	{
		if(getCurrentDisplayState() == DisplayState.SINGLE_REVIEW_FROM_GALLERY)
			return false;
		return super.isShowForward();
	}

	protected Context getContext()
	{
		return collectorUI.getContext();
	}

	protected void minimiseCaptureButton()
	{
		if(captureView != null)
			captureView.minimiseCaptureButton();
	}

	protected void maximiseCaptureButton()
	{
		if(captureView != null)
			captureView.maximiseCaptureButton();
	}
	
	/**
	 * Return a "discard" button item to be used when reviewing a single media item. Will use field-specific capture button if 
	 * available, otherwise the default resource is used. Can be overriden by the subclass if some kind of 'dynamic' discard 
	 * button is needed, analogous to the capture button.
	 * 
	 * @param context
	 * @return the "discard" button item
	 */
	public Item generateDiscardButton(Context context)
	{
		Item discardButton = null;
		File discardImgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getDiscardButtonImageRelativePath());
		if(FileHelpers.isReadableFile(discardImgFile))
			discardButton = new FileImageItem(discardImgFile);
		else
			discardButton = new ResourceImageItem(context.getResources(), R.drawable.button_trash_svg);
		discardButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return discardButton;
	}
	
	/**
	 * @return whether or not the capture button should be maximised when the capture UI is first entered (can be overriden
	 * by subclass, but returns {@code false} by default).
	 */
	protected boolean isMaximiseCaptureButton()
	{
		return false;
	}
	
	// -------- ABSTRACT METHODS:

	/**
	 * What to do when the capture button has been pressed.
	 * 
	 * @return a boolean indicating whether or not to immediately allow new user interactions (i.e. clicks)
	 * after this method has returned.
	 */
	protected abstract boolean onCapture();

	/**
	 * What to do when a piece of media is discarded after review (e.g. release media player resources).
	 */
	protected abstract void onDiscard();
	
	/**
	 * Returns an appropriate {@link Item} for the provided file.
	 * @param file - the file to use to create the item.
	 * @return an Item corresponding to the file.
	 */
	protected abstract Item getItemFromFile(File file);

	/**
	 * Generate a "capture" button (may vary by media, e.g. microphone
	 * for audio and camera for photo).
	 * @param context
	 * @return an ImageItem housing an appropriate "capture" button for the media.
	 */
	protected abstract Item generateCaptureButton(Context context);

	/**
	 * Creates the main content for the capture UI.
	 * @param context
	 * @return a {@code View} containing the content for capture mode.
	 */
	protected abstract View getCaptureContent(Context context);

	/**
	 * Creates the main content for the review UI.
	 * @param context
	 * @param mediaFile - the file to be reviewed.
	 * @return a {@code View} containing the content for review mode.
	 */
	protected abstract View getReviewContent(Context context, File mediaFile);
	
	/**
	 * Creates a {@code View} to be used as a (capture/discard/etc) button in the media UI.
	 * @param context
	 * @param buttonItem - the {@code Item} from which to create the button.
	 * @param onClickRunnable - the {@code Runnable} to execute when the button is clicked.
	 * @return the button as a {@code View}.
	 */
	private View buttonFromItem(Context context, Item buttonItem, final Runnable onClickRunnable)
	{
		final View view = buttonItem.getView(context);
		view.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				controller.blockUI();
				// Execute the "press" animation if allowed, then perform the action:
				if(controller.getCurrentForm().isClickAnimation())
					ClickAnimator.Animate(onClickRunnable, view, controller); // execute animation and the action afterwards
				else
					onClickRunnable.run(); // perform task now (animation is disabled)

			}
		});

		return view;
	}
	
	// -------------- CAPTURE UI:

	/**
	 * LinearLayout that holds the capture UI.
	 * @author benelliott
	 *
	 */
	private class CaptureView extends LinearLayout
	{

		private View contentView; // capture UI content (e.g. viewfinder)
		private View buttonView; // capture button
		private Runnable buttonAction; // capture button action
		private boolean captureButtonMaximised = false;
		private Context context;

		private CaptureView(Context context, boolean maximiseCaptureButton)
		{
			super(context);
			this.context = context;
			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			// set weight sum (capture UI content will expand to fill all 'spare' space:
			this.setWeightSum(1.0f);

			// add content:
			this.contentView = getCaptureContent(context);
			LinearLayout.LayoutParams contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.gravity = Gravity.CENTER_HORIZONTAL;
			contentParams.weight = 1.0f;
			addView(contentView, contentParams);

			// add button:
			buttonAction = new Runnable()
			{
				@Override
				public void run()
				{
					// just made a capture, so go to gallery/review when the field is re-entered:
					controller.getCurrentFieldArguments().remove(GO_TO_CAPTURE_KEY);
					// execute subclass's capture behaviour and make note of whether or not to unblock UI afterwards
					boolean unblock = onCapture();
					// refresh capture button on press (e.g. might need to change from "record" to "stop recording")
					refreshCaptureButton(null);
					// force gallery to check for new files next time it is entered:
					mediaItemsChanged = true;
					if(unblock)
						// unblock UI if requested by subclass
						controller.unblockUI();
				}
			};

			buttonView = buttonFromItem(context, generateCaptureButton(context), buttonAction);
			addView(buttonView, buttonParams);

			// maximise capture button initially, if required:
			if(maximiseCaptureButton)
				maximiseCaptureButton();
		}

		/**
		 * Restores a maximised capture button to its original position at the bottom of the screen.
		 */
		protected void minimiseCaptureButton()
		{
			if(captureButtonMaximised)
			{
				captureButtonMaximised = false;
				// show all other content from capture UI:
				contentView.setVisibility(View.VISIBLE);
				// redraw the capture button:
				refreshCaptureButton(buttonParams);
			}
		}

		/**
		 * Expands a minimised capture button to take up the entire capture UI (e.g. for audio recording).
		 */
		protected void maximiseCaptureButton()
		{
			if(!captureButtonMaximised)
			{
				captureButtonMaximised = true;
				// hide all other content from capture UI:
				contentView.setVisibility(View.GONE);
				// redraw the capture button:
				refreshCaptureButton(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT));
			}
		}
		
		/**
		 * Recreate the capture button with the specified layout parameters.
		 * @param newLayoutParams - the new layout parameters that should be applied to the button. If 
		 * {@code null} then the previous parameters will be used.
		 */
		private void refreshCaptureButton(ViewGroup.LayoutParams newLayoutParams)
		{
			if(newLayoutParams == null) // use whatever params it had before:
				newLayoutParams = buttonView.getLayoutParams();

			this.removeView(buttonView);
			buttonView = buttonFromItem(context, generateCaptureButton(context), buttonAction);
			this.addView(buttonView, newLayoutParams);
		}
	}
	
	/**
	 * LinearLayout that holds the single-item review UI.
	 * @author benelliott
	 *
	 */
	private class ReviewView extends LinearLayout
	{

		private View buttonView; // "delete item" button
		private File toReview; // file used to populate review UI

		private ReviewView(Context context, File toReview)
		{
			super(context);
			this.toReview = toReview;
			// layout:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);

			// add content:
			View contentView = getReviewContent(context, toReview);
			LinearLayout.LayoutParams contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.weight = 1.0f;
			contentParams.gravity = Gravity.CENTER;
			addView(contentView, contentParams);

			// add button:
			final Runnable buttonAction = new Runnable()
			{
				@Override
				public void run()
				{
					// only one button: discard
					onDiscard();
					// captures are now always attached, so must be deleted regardless of approval:
					removeMedia(ReviewView.this.toReview);
					// if gallery cache is being used, remove this item from it now it has been deleted:
					if(galleryCache != null)
						galleryCache.remove(ReviewView.this.toReview);
					// have now deleted media, so cannot have reached max
					maxReached = false;
					// an item has been deleted, so want the gallery to be refreshed:
					mediaItemsChanged = true;

					// remove the filepath from the field's arguments so we do not re-enter single-item review unintentionally
					controller.getCurrentFieldArguments().remove(REVIEW_FILE_PATH_KEY);

					if(field.getCount(controller.getCurrentRecord()) < 1)
						// go to capture if no attachments remain
						controller.getCurrentFieldArguments().put(GO_TO_CAPTURE_KEY, "true");

					// either go back to capture, or go back to gallery:
					controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
					// allow clicks now we have finished
					controller.unblockUI();
				}
			};
			
			buttonView = buttonFromItem(context, generateDiscardButton(context), buttonAction);
			addView(buttonView, buttonParams);
		}
	}
	
	/**
	 * LinearLayout that holds the gallery UI (only used when there are multiple media attachments to show).
	 * 
	 * @author benelliott
	 *
	 */
	private class GalleryView extends LinearLayout
	{

		private GalleryPicker pickerView;
		private View buttonView;
		private Context context;
		private Runnable buttonAction;

		private GalleryView(Context context)
		{
			super(context);
			this.context = context;
			// layout parameters:
			this.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			this.setOrientation(LinearLayout.VERTICAL);
			this.setWeightSum(1.0f);

			// add gallery picker:
			pickerView = new GalleryPicker(context);
			LinearLayout.LayoutParams contentParams = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
			contentParams.weight = 1.0f;
			this.addView(pickerView, contentParams);

			// add button:
			buttonAction = new Runnable()
			{
				@Override
				public void run()
				{
					// "capture more" button clicked, return to camera interface
					if(!maxReached)
					{
						// add the "go to capture" argument to the field:
						controller.getCurrentFieldArguments().put(GO_TO_CAPTURE_KEY, "true");
						// re-enter current field:
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
					}
					// allow clicks now we have finished
					controller.unblockUI();
				}
			};

			buttonView = buttonFromItem(context, createCaptureMoreButton(), buttonAction);
			this.addView(buttonView, buttonParams);
		}

		/**
		 * Refreshes the contents of the gallery and the "capture" button.
		 */
		private void refresh()
		{
			pickerView.loadMedia();
			// refresh capture button in case it is now (un-)greyed out:
			this.removeView(buttonView);
			buttonView = buttonFromItem(context, createCaptureMoreButton(), buttonAction);
			this.addView(buttonView, buttonParams);
		}

		/**
		 * Creates a "capture more" button to be used in the picker, but makes it appear greyed out
		 * if the field's maximum number of attachments has been reached.
		 * 
		 * @return the "capture more" button to be added to the UI.
		 */
		private Item createCaptureMoreButton()
		{
			// creates a "normal" capture button and then disables it if max reached.

			Item captureButton = generateCaptureButton(getContext());

			if(!maxReached)
				return captureButton;

			// else make button look unclickable
			LayeredItem layeredItem = new LayeredItem();
			layeredItem.addLayer(captureButton, false);

			// Make background of layered stack gray:
			layeredItem.setBackgroundColor(CollectorView.COLOR_GRAY);
			// Add grayed-out layer:
			Item grayOutOverlay = new EmptyItem();
			grayOutOverlay.setBackgroundColor(CollectorView.COLOR_MOSTLY_OPAQUE_GRAY);
			layeredItem.addLayer(grayOutOverlay, false);

			return layeredItem;
		}
		
		/**
		 * A PickerView which allows for the review of multiple media items, such as
		 * images or audio.
		 * 
		 * @author benelliott
		 */
		private class GalleryPicker extends PickerView
		{
			private static final int NUM_COLUMNS = 3; // TODO make configurable?
			private static final int NUM_ROWS = 3;

			private GalleryPicker(Context context)
			{
				super(context);

				LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, 0);
				params.weight = 1.0f;
				this.setLayoutParams(params);

				// Number of columns:
				setNumColumns(NUM_COLUMNS);
				// Item size & padding:
				setItemDimensionsPx(collectorUI.getFieldUIPartWidthPx(NUM_COLUMNS), collectorUI.getFieldUIPartHeightPx(NUM_ROWS));

				// Item spacing:
				int spacingPx = collectorUI.getSpacingPx();
				setHorizontalSpacing(spacingPx);
				setVerticalSpacing(spacingPx);

				// When a gallery item is clicked, present it in full screen for review/deletion:
				setOnItemClickListener(new OnItemClickListener()
				{
					@Override
					public void onItemClick(AdapterView<?> parent, View view, int position, long id)
					{
						controller.clickView(view, getItemClickRunnable(position));
					}
				});
			}
			
			/**
			 * Convenience method for creating a {@code Runnable} that passes the clicked item to the full-page review screen.
			 * @param position - the index of the item in the gallery
			 * @return the {@code Runnable} that should be executed when this item is clicked.
			 */
			private Runnable getItemClickRunnable(final int position)
			{
				return new Runnable()
				{
					@Override
					public void run()
					{
						controller.getCurrentFieldArguments().put(REVIEW_FILE_PATH_KEY,
								((FileItem) GalleryPicker.this.getAdapter().getItem(position)).getFile().getAbsolutePath());
						controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
					}
				};
			}

			/**
			 * Load the media items into the adapter and then refresh the View.
			 */
			private void loadMedia()
			{
				PickerAdapter adapter;

				if(mediaItemsChanged)
				{
					// an item has been added or deleted so reload gallery items
					// Log.d(TAG, "Media items changed, updating gallery...");
					// reset adapter
					adapter = new PickerAdapter();
					for(File file : field.getAttachments(controller.getFileStorageProvider(), controller.getCurrentRecord()))
					{
						// for each file in this field's attachments...

						// see if it can be found in the gallery cache:
						Item toAdd = galleryCache.get(file);

						if(toAdd == null)
						{
							// if not, create a new item and put it in the cache:
							toAdd = getItemFromFile(file);
							galleryCache.put(file, toAdd);
						}
						// set padding and add item to adapter
						toAdd.setPaddingDip(CollectorView.PADDING_DIP);
						adapter.addItem(toAdd);
					}

					if(adapter.getCount() >= field.getMax())
						maxReached = true;

					mediaItemsChanged = false;
				}
				else
					// else just use the existing adapter
					adapter = getAdapter();

				// force the picker to update its views
				setAdapter(adapter);
			}
		}
	}
	
	
	/**
	 * View for displaying a MediaField as part of a Page
	 * 
	 * @author mstevens, benelliott
	 */
	public class OnPageView extends LinearLayout implements OnClickListener, OnFocusChangeListener
	{
		static public final float PAGE_ITEM_SIZE_DIP = 60.0f; // width = height
		static public final float PAGE_ITEM_MARGIN_DIP = 1.0f; // same margin all round

		private TextView label;
		private View captureView;
		private int captureSizePx;
		private int captureMarginPx;

		public OnPageView(Context context)
		{
			super(context);
			this.setOrientation(LinearLayout.VERTICAL);

			// Add label:
			label = new TextView(getContext());
			label.setText(field.getCaption());
			// ensure that the label text is not truncated, by setting width to WRAP_CONTENT:
			label.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
			this.addView(label);

			captureSizePx = ScreenMetrics.ConvertDipToPx(context, PAGE_ITEM_SIZE_DIP);
			captureMarginPx = ScreenMetrics.ConvertDipToPx(context, PAGE_ITEM_MARGIN_DIP);

			// just use the capture button image to represent the media field:
			captureView = generateCaptureButton(context).getView(context);

			// Set margins on layoutparams:
			LayoutParams captureLP = new LinearLayout.LayoutParams(captureSizePx, captureSizePx);
			captureLP.setMargins(captureMarginPx, captureMarginPx, captureMarginPx, captureMarginPx);

			// Add the view:
			this.addView(captureView, captureLP);
		}

		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			if(captureView != null)
			{
				captureView.setEnabled(enabled);
				captureView.setOnClickListener(enabled ? this : null);
				// Make other fields lose focus and simulate clicking with onFocusChange:
				captureView.setFocusable(enabled);
				captureView.setFocusableInTouchMode(enabled);
				captureView.setOnFocusChangeListener(enabled ? this : null);
			}
		}

		@Override
		public void onFocusChange(View v, boolean hasFocus)
		{ //TODO necessary?
			if(hasFocus && isFieldShown() && isEnabled() && v.isEnabled())
			{
				// Lose focus again:
				v.clearFocus();
				
				// Simulate click:
				onClick(v);
			}
		}

		@Override
		public void onClick(View v)
		{
			// Do nothing if not shown or enabled:
			if(!isFieldShown() || !isEnabled() || !v.isEnabled())
				return;
			
			clearPageInvalidMark();
			
			// Task to perform after animation has finished:
			Runnable action = new Runnable()
			{
				public void run()
				{
					controller.goTo(new FieldWithArguments(field), LeaveRule.UNCONDITIONAL_NO_STORAGE); // force leaving of the page, to go to the field itself
				}
			};

			// Perform the click
			controller.clickView(v, action);
		}
		
	}
}
