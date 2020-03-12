/**
 * Sapelli data collection platform: http://sapelli.org
 * <p>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.util;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.Intent;
import android.content.pm.ShortcutInfo;
import android.content.pm.ShortcutManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.Icon;
import android.os.Build;

import java.io.File;
import java.util.Collections;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.media.MediaHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.ResourcesHelpers;

/**
 * @author mstevens, Michalis Vitos
 */
public class ProjectRunHelpers {

    // SHORTCUT INTENT ACTIONS
    public static final String DEFAULT_INSTALL_SHORTCUT_ACTION = "com.android.launcher.action.INSTALL_SHORTCUT";
    public static final String DEFAULT_UNINSTALL_SHORTCUT_ACTION = "com.android.launcher.action.UNINSTALL_SHORTCUT";
    public static final String SAPELLI_LAUNCHER_INSTALL_SHORTCUT_ACTION = "uk.ac.ucl.excites.sapelli.launcher.INSTALL_SHORTCUT";
    public static final String SAPELLI_LAUNCHER_UNINSTALL_SHORTCUT_ACTION = "uk.ac.ucl.excites.sapelli.launcher.UNINSTALL_SHORTCUT";

    // SHORTCUT INTENT PARAMETER FOR SAPELLI LAUNCHER
    public static final String SAPELLI_LAUNCHER_SHORTCUT_ICON_PATH = "uk.ac.ucl.excites.sapelli.launcher.shortcut.ICON_PATH";

    /**
     * Creates an intent to start the CollectorActivity, with given project, *not* from a shortcut
     *
     * @param contextWrapper
     * @param project
     * @return
     */
    static public Intent getProjectRunIntent(Context contextWrapper, Project project) {
        return getProjectRunIntent(contextWrapper, project, null);
    }

    /**
     * Creates an intent to start the CollectorActivity, with given project, from a shortcut
     *
     * @param contextWrapper
     * @param projDescr
     * @param shortcutName
     * @return
     */
    static private Intent getProjectRunIntent(Context contextWrapper, ProjectDescriptor projDescr, String shortcutName) {
        Intent i = new Intent(contextWrapper.getApplicationContext(), CollectorActivity.class);
        i.putExtra(CollectorActivity.INTENT_PARAM_PROJECT_ID, projDescr.getID());
        i.putExtra(CollectorActivity.INTENT_PARAM_PROJECT_FINGERPRINT, projDescr.getFingerPrint());
        if (shortcutName != null)
            i.putExtra(CollectorActivity.INTENT_OPTIONAL_PARAM_SHORTCUT_NAME, shortcutName);
        i.setAction(Intent.ACTION_MAIN);
        return i;
    }

    /**
     * Add a shortcut for the Sapelli Collector Project Manager to the Home screen
     *
     * @param context
     */
    static public void createCollectorShortcut(Context context) {
        // Adding shortcut for ProjectManagerActivity
        Intent shortcutIntent = new Intent(context, ProjectManagerActivity.class);
        shortcutIntent.setAction(Intent.ACTION_MAIN);

        Intent addIntent = new Intent();
        addIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, shortcutIntent);
        addIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, context.getString(R.string.app_name));
        addIntent.putExtra(Intent.EXTRA_SHORTCUT_ICON_RESOURCE, Intent.ShortcutIconResource.fromContext(context, R.drawable.ic_sapelli_logo));

        addIntent.setAction("com.android.launcher.action.INSTALL_SHORTCUT");
        context.sendBroadcast(addIntent);
    }

    /**
     * @param fileStorageProvider
     * @param project
     * @return
     */
    static public File getShortcutImageFile(FileStorageProvider fileStorageProvider, Project project) {
        return fileStorageProvider.getProjectImageFile(project, project.getStartForm().getShortcutImageRelativePath()); // use icon of the startForm
    }

    /**
     * @param contextWrapper
     * @param fileStorageProvider
     * @param project
     */
    static public Drawable getShortcutDrawable(ContextWrapper contextWrapper, FileStorageProvider fileStorageProvider, Project project) {
        File shortcutImageFile = getShortcutImageFile(fileStorageProvider, project);
        return (FileHelpers.isReadableFile(shortcutImageFile) && MediaHelpers.isRasterImageFileName(shortcutImageFile.getName())) ?
                Drawable.createFromPath(shortcutImageFile.getAbsolutePath()) :
                ResourcesHelpers.getDrawable(contextWrapper.getResources(), R.drawable.ic_sapelli_logo);
    }

    /**
     * @param contextWrapper
     * @param fileStorageProvider
     * @param project
     */
    static public Bitmap getShortcutBitmap(ContextWrapper contextWrapper, FileStorageProvider fileStorageProvider, Project project) {
        File shortcutImageFile = getShortcutImageFile(fileStorageProvider, project);
        return (FileHelpers.isReadableFile(shortcutImageFile) && MediaHelpers.isRasterImageFileName(shortcutImageFile.getName())) ?
                BitmapFactory.decodeFile(shortcutImageFile.getAbsolutePath()) :
                BitmapFactory.decodeResource(contextWrapper.getResources(), R.drawable.ic_sapelli_logo);
    }

    /**
     * Create shortcut(s) to open project in CollectorActivity
     *
     * @param contextWrapper
     * @param fileStorageProvider
     * @param project
     */
    static public void createShortcut(ContextWrapper contextWrapper, FileStorageProvider fileStorageProvider, Project project) {

        // Shortcut name:
        String shortcutName = project.toString();

        // Get up icon bitmap (using build-in Sapelli icon if the shortcut file doesn't exist or is not a raster image):
        Drawable iconResource = getShortcutDrawable(contextWrapper, fileStorageProvider, project);

        // Shortcut intent (starting CollectorActivity with given project):
        Intent shortcutIntent = getProjectRunIntent(contextWrapper, project, shortcutName);

        if (Build.VERSION.SDK_INT >= 26) {
            ShortcutManager shortcutManager = contextWrapper.getSystemService(ShortcutManager.class);

            // If shortcut already exists and disabled, enable it
            if (shortcutManager != null) {
                for (ShortcutInfo shortCut : shortcutManager.getPinnedShortcuts()) {
                    if (!shortcutName.equals(shortCut.getShortLabel())) continue;

                    if (!shortCut.isEnabled()){
                        shortcutManager.enableShortcuts(Collections.singletonList(shortcutName));
                        return;
                    }
                }
            }

            // Add shortcut
            if (shortcutManager.isRequestPinShortcutSupported()) {
                ShortcutInfo pinShortcutInfo = new ShortcutInfo
                        .Builder(contextWrapper, shortcutName)
                        .setIcon(Icon.createWithBitmap(((BitmapDrawable) iconResource).getBitmap()))
                        .setIntent(shortcutIntent)
                        .setShortLabel(shortcutName)
                        .build();

                shortcutManager.requestPinShortcut(
                        pinShortcutInfo, null
                );
            }
            return;
        }

        //-----------------------------------------------------
        // Create a shortcut in standard Android Home Launcher
        //-----------------------------------------------------
        Intent androidLauncherIntent = getShortcutCreationIntent(contextWrapper, shortcutName, shortcutIntent, false);
        Bitmap icon = ((BitmapDrawable) iconResource).getBitmap();
        // Resize the icon bitmap according to the default size:
        int maxIconSize = (int) contextWrapper.getResources().getDimension(android.R.dimen.app_icon_size); // Get standard system icon size
        if (icon.getWidth() > maxIconSize || icon.getHeight() > maxIconSize)
            icon = Bitmap.createScaledBitmap(icon, maxIconSize, maxIconSize, true); //TODO make this keep aspect ratio?
        // Set up shortcut icon:
        androidLauncherIntent.putExtra(Intent.EXTRA_SHORTCUT_ICON, icon);
        // Fire the intent:
        contextWrapper.sendBroadcast(androidLauncherIntent);
        //-----------------------------------------------------

        //-----------------------------------------------------
        // Create an shortcut in the Sapelli Launcher
        //-----------------------------------------------------
        Intent sapelliLauncherIntent = getShortcutCreationIntent(contextWrapper, shortcutName, shortcutIntent, true);
        // Set up shortcut icon path:
        File shortcutImageFile = getShortcutImageFile(fileStorageProvider, project);
        sapelliLauncherIntent.putExtra(SAPELLI_LAUNCHER_SHORTCUT_ICON_PATH, FileHelpers.isReadableFile(shortcutImageFile) ? shortcutImageFile.getAbsolutePath() : null); // launcher will use default Sapelli icon when path is null
        // Fire the intent:
        contextWrapper.sendBroadcast(sapelliLauncherIntent);
        //-----------------------------------------------------
    }

    /**
     * @param contextWrapper
     * @param shortcutName
     * @param shortcutIntent
     * @param sapelliLauncher
     * @return
     */
    static private Intent getShortcutCreationIntent(ContextWrapper contextWrapper, String shortcutName, Intent shortcutIntent, boolean sapelliLauncher) {
        Intent shortcutCreationIntent = new Intent();

        // Action:
        shortcutCreationIntent.setAction(sapelliLauncher ? SAPELLI_LAUNCHER_INSTALL_SHORTCUT_ACTION : DEFAULT_INSTALL_SHORTCUT_ACTION);
        // Shortcut name:
        shortcutCreationIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, shortcutName);
        // Shortcut intent:
        shortcutCreationIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, shortcutIntent);
        // 	Do not allow duplicate shortcuts:
        if (!sapelliLauncher)
            shortcutCreationIntent.putExtra("duplicate", false); // only needed for Android Home Launcher (although Sapelli Launcher would just ignore it)

        return shortcutCreationIntent;
    }

    /**
     * Remove shortcut(s) to open project in CollectorActivity
     *
     * @param contextWrapper
     * @param projDescr
     */
    static public void removeShortcut(ContextWrapper contextWrapper, ProjectDescriptor projDescr) {
        // Shortcut name:
        String shortcutName = projDescr.toString();

        // Remove in all launchers:
        removeShortcut(contextWrapper, shortcutName, getProjectRunIntent(contextWrapper, projDescr, shortcutName));
    }

    /**
     * Remove shortcut(s) to open project in CollectorActivity
     *
     * @param contextWrapper
     * @param shortcutName
     * @param shortcutIntent
     */
    static public void removeShortcut(ContextWrapper contextWrapper, String shortcutName, Intent shortcutIntent) {
        // Just in case:
        if (shortcutName == null || shortcutName.isEmpty() || shortcutIntent == null)
            return;

        // Remove a shortcut from the standard Android Home Launcher
        contextWrapper.sendBroadcast(getShortcutRemovalIntent(contextWrapper, shortcutName, shortcutIntent, false));

        // Remove a shortcut from the Sapelli Launcher
        contextWrapper.sendBroadcast(getShortcutRemovalIntent(contextWrapper, shortcutName, shortcutIntent, true));

        if (Build.VERSION.SDK_INT >= 26) {
            ShortcutManager shortcutManager = contextWrapper.getSystemService(ShortcutManager.class);

            if (shortcutManager != null) {
                shortcutManager.disableShortcuts(Collections.singletonList(shortcutName));
            }
        }

    }

    /**
     * @param contextWrapper
     * @param shortcutName
     * @param shortcutIntent
     * @param sapelliLauncher
     * @return
     */
    static private Intent getShortcutRemovalIntent(ContextWrapper contextWrapper, String shortcutName, Intent shortcutIntent, boolean sapelliLauncher) {
        Intent shortcutRemovalIntent = new Intent();

        // Action:
        shortcutRemovalIntent.setAction(sapelliLauncher ? SAPELLI_LAUNCHER_UNINSTALL_SHORTCUT_ACTION : DEFAULT_UNINSTALL_SHORTCUT_ACTION);
        // Shortcut intent:
        shortcutRemovalIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, shortcutIntent);
        // Shortcut name:
        shortcutRemovalIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, shortcutName);

        return shortcutRemovalIntent;
    }

}
