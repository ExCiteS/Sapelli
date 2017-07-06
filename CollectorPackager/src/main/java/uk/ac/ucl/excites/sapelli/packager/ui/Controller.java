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

package uk.ac.ucl.excites.sapelli.packager.ui;


import com.jfoenix.controls.JFXButton;

import java.io.File;
import java.text.MessageFormat;
import java.util.List;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Color;
import javafx.scene.text.TextFlow;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;
import jiconfont.icons.FontAwesome;
import jiconfont.javafx.IconFontFX;
import jiconfont.javafx.IconNode;
import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.packager.io.ProjectWatcher;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectChecker;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectUtils;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectZipper;
import uk.ac.ucl.excites.sapelli.packager.ui.UiMessageManager.State;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;

@Slf4j
public class Controller
{

	// Privates:
	private ProjectChecker projectChecker;
	private UiMessageManager uiMessageManager;
	private ProjectWatcher sapelliProjectWatcher;

	@FXML
	private ResourceBundle resources;

	//----- UI

	//--- Main elements
	@FXML
	public AnchorPane root;

	//--- Working directory elements
	@FXML
	public AnchorPane workingDirectoryBackground;
	@FXML
	private Label workingDirectoryLabel;

	//--- Warnings/messages elements
	@FXML
	public Label warningLabel;
	@FXML
	public TextFlow messageLabel;

	//--- Packager
	@FXML
	private JFXButton packageButton;
	@FXML
	private JFXButton buttonAbout;

	@FXML
	public void initialize()
	{
		// Do any initialisation required here
		uiMessageManager = new UiMessageManager(resources);

		// Register FontAwesome
		IconFontFX.register(FontAwesome.getIconFont());

		// Create About icons
		IconNode aboutIcon = new IconNode(FontAwesome.INFO);
		aboutIcon.setIconSize(18);
		aboutIcon.setFill(Color.WHITE);

		buttonAbout.setGraphic(aboutIcon);
		buttonAbout.setContentDisplay(ContentDisplay.RIGHT);
	}

	/**
	 * Method to be called when the Browse button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onBrowseButtonClicked(ActionEvent actionEvent)
	{
		// Get the Directory Chooser
		final Stage stage = (Stage) root.getScene().getWindow();
		final DirectoryChooser directoryChooser = new DirectoryChooser();
		if(projectChecker != null && projectChecker.sapelliProjectDirExists())
			directoryChooser.setInitialDirectory(projectChecker.getSapelliProjectDir());
		directoryChooser.setTitle(resources.getString("directory_chooser"));
		final File sapelliProjectDir = directoryChooser.showDialog(stage);

		// Ensure that the user has selected something
		if(sapelliProjectDir == null)
			return;

		// Create a new Project Checker
		projectChecker = new ProjectChecker(sapelliProjectDir);

		// Validate Working directory and Project
		if(validateWorkingDirectory())
			validateProject();
	}

	/**
	 * Method to be called when the Package button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onPackageButtonClicked(ActionEvent actionEvent)
	{
		// Reset the uiMessageManager
		uiMessageManager.clear();
		// Let's assume that this will be a success:
		uiMessageManager.setState(State.SUCCESS);

		try
		{
			// Create a ProjectZipper:
			ProjectZipper projectZipper = new ProjectZipper(projectChecker);

			// Check if file already exists:
			File previousFile = projectZipper.getSapFile();
			if(previousFile.exists())
			{
				// Create the backup
				String backupFileName = FileHelpers.trimFileExtensionAndDot(previousFile.toString());
				backupFileName += "-" + TimeUtils.getTimestampForFileName() + "." + ProjectZipper.SAP_EXTENSION;

				File backupFile = new File(backupFileName);

				if(previousFile.renameTo(backupFile))
				{
					uiMessageManager.setState(State.WARNING);
					uiMessageManager.addMessage(MessageFormat.format(resources.getString("sap_already_in_directory"), backupFile.getName()));
				}
				else
				{
					uiMessageManager.setState(State.ERROR);
					uiMessageManager.addMessage(resources.getString("sap_already_in_directory_cannot_backup"));
				}
			}

			// Zip the project
			projectZipper.zipProject();

			uiMessageManager.addMessage(MessageFormat.format(resources.getString("sap_packaged_into"), projectZipper.getSapFile()));

			// Log
			log.info(uiMessageManager.toString());
		}
		catch(Exception e)
		{
			uiMessageManager.setState(State.ERROR);
			uiMessageManager.addMessage(MessageFormat.format(resources.getString("sap_package_error"), e.getLocalizedMessage()));

			log.error("Error while Zipping project", e);
		}

		// Update the UI
		updateUI();
	}

	/**
	 * Method to be called when the Package button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onAboutButtonClicked(ActionEvent actionEvent)
	{
		// TODO: 06/07/2017  
	}

	/**
	 * Validate the Working Directory and update the UI
	 *
	 * @return true if we have a valid directory
	 */
	private boolean validateWorkingDirectory()
	{
		// 1: Update Working Dir Or return
		if(projectChecker.sapelliProjectDirExists())
		{
			// Set style
			workingDirectoryBackground.getStyleClass().clear();
			workingDirectoryBackground.getStyleClass().addAll("box-with-padding", "working-dir-success");

			// Set text
			workingDirectoryLabel.setText(projectChecker.getSapelliProjectDir().toString());
		}
		else
		{
			// Set style
			workingDirectoryBackground.getStyleClass().clear();
			workingDirectoryBackground.getStyleClass().addAll("box-with-padding", "working-dir-error");

			// Set text and exit
			workingDirectoryLabel.setText(resources.getString("directory_chooser"));
			return false;
		}

		return projectChecker.sapelliProjectDirExists();
	}

	/**
	 * Validate the Project and update the UI
	 */
	private void validateProject()
	{
		// Monitor any file changes
		watchDirectoryForChanges();

		// Reset the uiMessageManager
		uiMessageManager.clear();

		// 2: Check if PROJECT.XML exists
		if(projectChecker.projectXmlExists())
		{
			// Get Project, Warnings and Errors
			final Project project = projectChecker.getProject();
			final List<String> warnings = projectChecker.getWarnings();
			final List<String> errors = projectChecker.getErrors();
			final List<String> missing = projectChecker.getMissingFiles();

			// CASE 1: SUCCESS
			// Project exists, no warnings, no errors, no missing files
			if(project != null && warnings.isEmpty() && errors.isEmpty() && missing.isEmpty())
			{
				uiMessageManager.setState(State.SUCCESS);
				uiMessageManager.addProject(ProjectUtils.printProjectInfo(project));

				// Enable the Package button
				packageButton.setDisable(false);
			}
			// CASE 2: WARNINGS
			// Project exists with warnings or missing files, no errors
			if(project != null && (!warnings.isEmpty() || !missing.isEmpty()) && errors.isEmpty())
			{
				// Disable the Package button
				packageButton.setDisable(true);

				uiMessageManager.setState(State.WARNING);
				uiMessageManager.addWarnings(warnings);
				uiMessageManager.addMissingFiles(missing);
			}

			// CASE 3: ERROR
			// Project does not exists or there are errors
			if(project == null || !errors.isEmpty())
			{
				// Disable the Package button
				packageButton.setDisable(true);

				uiMessageManager.setState(State.ERROR);
				uiMessageManager.addErrors(errors);
			}

			// TODO: 02/06/2017 Continue this?
		}
		else
		{
			// Inform the user that the PROJECT.XML does not exist
			uiMessageManager.setState(State.ERROR);
			// Set text to inform user
			uiMessageManager.addMessage(MessageFormat.format(resources.getString("project_xml_missing"), projectChecker.getSapelliProjectDir().toString()));

			// Disable the Package button
			packageButton.setDisable(true);
		}


		// TODO: 01/06/2017 Continue the validation here


		// Finally, update the UI
		updateUI();
	}

	/**
	 * Watch the Sapelli Dir for file changes and refresh UI
	 */
	private void watchDirectoryForChanges()
	{
		// Stop Previous Watcher
		if(sapelliProjectWatcher != null)
		{
			sapelliProjectWatcher.stopWatching();
			sapelliProjectWatcher = null;
		}

		sapelliProjectWatcher = new ProjectWatcher(projectChecker, path ->
		{
			// We want this to run to UI thread
			Platform.runLater(() ->
			{
				log.info("'{}' has been changed. Refresh UI.", path.getFileName());

				if(projectChecker == null)
					return;

				projectChecker.refresh();

				// Clear UI
				uiMessageManager.clear();

				// Update UI
				validateProject();
			});
		});
	}

	/**
	 * Update UI
	 */
	private void updateUI()
	{
		// Update UI
		switch(uiMessageManager.getState())
		{
			case DEFAULT:
				warningLabel.getStyleClass().clear();
				warningLabel.getStyleClass().add("warning-default");
				break;
			case SUCCESS:
				warningLabel.getStyleClass().clear();
				warningLabel.getStyleClass().add("warning-success");
				break;
			case WARNING:
				warningLabel.getStyleClass().clear();
				warningLabel.getStyleClass().add("warning-warning");
				break;
			case ERROR:
				warningLabel.getStyleClass().clear();
				warningLabel.getStyleClass().add("warning-error");
				break;
		}

		// Clear old messages and add new
		messageLabel.getChildren().clear();
		messageLabel.getChildren().addAll(uiMessageManager.getMessages());
	}

	/**
	 * Method to be called when the Stage is about to terminate and clean up any resources
	 */
	public void close()
	{
		log.info("Cleaning up resources in the Controller.");

		// Stop Watching for file changes
		if(sapelliProjectWatcher != null)
			sapelliProjectWatcher.stopWatching();
	}
}
