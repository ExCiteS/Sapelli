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


import java.io.File;
import java.util.List;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectChecker;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectUtils;

@Slf4j
public class Controller
{

	// Directory for the Project Packager to work
	private ProjectChecker projectChecker;

	// UI
	@FXML
	public AnchorPane anchorPanelDirectory;
	@FXML
	private Button buttonBrowse;
	@FXML
	private Button buttonPackage;
	@FXML
	private Label labelDirectory;
	@FXML
	private AnchorPane anchorPane;
	@FXML
	private Label labelResult;
	@FXML
	public Accordion accordion;
	@FXML
	private TitledPane accordionProjectXML;
	@FXML
	private TitledPane accordionImg;
	@FXML
	private TitledPane accordionSnd;
	@FXML
	private TitledPane accordionResources;
	@FXML
	public Label labelProjectXML;


	/**
	 * Method to be called when the Browse button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onBrowseButtonClicked(ActionEvent actionEvent)
	{
		// Get the Directory Chooser
		final Stage stage = (Stage) anchorPane.getScene().getWindow();
		final DirectoryChooser directoryChooser = new DirectoryChooser();
		if(projectChecker != null && projectChecker.sapelliProjectDirExists())
			directoryChooser.setInitialDirectory(projectChecker.getSapelliProjectDir());
		directoryChooser.setTitle("Select directory of project");
		final File sapelliProjectDir = directoryChooser.showDialog(stage);

		// Ensure that the user has selected something
		if(sapelliProjectDir == null)
			return;

		// Create a new Project Checker
		projectChecker = new ProjectChecker(sapelliProjectDir);

		// Update UI
		updateUI();
	}

	/**
	 * Method to be called when the Refresh button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onRefreshButtonClicked(ActionEvent actionEvent)
	{
		if(projectChecker == null)
			return;

		projectChecker.refresh();

		// Update UI
		updateUI();
	}

	/**
	 * Method to be called when the Package button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onPackageButtonClicked(ActionEvent actionEvent)
	{
		log.info("Package button has been clidked: {}", actionEvent);

		// TODO: 05/06/2017 ZIP files
	}

	/**
	 * Update the UI
	 */
	private void updateUI()
	{
		// TODO: 26/05/2017 Reset all UI elements

		// 1: Update Working Dir Or return
		if(projectChecker.sapelliProjectDirExists())
		{
			// Set style
			anchorPanelDirectory.getStyleClass().clear();
			anchorPanelDirectory.getStyleClass().add("working-dir-success");

			// Set text
			labelDirectory.setText(projectChecker.getSapelliProjectDir().toString());
		}
		else
		{
			// Set style
			anchorPanelDirectory.getStyleClass().clear();
			anchorPanelDirectory.getStyleClass().add("working-dir-error");

			// Set text and exit
			labelDirectory.setText("Please select a directory...");
			return;
		}

		// 2: Check if PROJECT.XML exists
		if(projectChecker.projectXmlExists())
		{
			// Expand the Accordion
			accordion.setExpandedPane(accordionProjectXML);

			// Get Project, Warnings and Errors
			final Project project = projectChecker.getProject();
			final List<String> warnings = projectChecker.getWarnings();
			final List<String> errors = projectChecker.getErrors();

			// CASE 1: SUCCESS
			// Project exists, no warnings, no errors
			if(project != null && warnings.isEmpty() && errors.isEmpty())
			{
				setSuccessTitledPaneStyle(accordionProjectXML);
				labelProjectXML.setText(ProjectUtils.printProjectInfo(project));

				// Enable the Package button
				buttonPackage.setDisable(false);
			}
			// CASE 2: WARNINGS
			// Project exists with warnings, no errors
			if(project != null && !warnings.isEmpty() && errors.isEmpty())
			{
				setWarningTitledPaneStyle(accordionProjectXML);

				// Disable the Package button
				buttonPackage.setDisable(true);

				// TODO: Fix this
				labelProjectXML.setText(warnings.toString());
			}

			// CASE 3: ERROR
			// Project does not exists or there are errors
			if(project == null || !errors.isEmpty())
			{
				setErrorTitledPaneStyle(accordionProjectXML);

				// Disable the Package button
				buttonPackage.setDisable(true);

				// TODO: Fix this
				labelProjectXML.setText(errors.toString());
			}

			// TODO: 02/06/2017 Continue this?
		}
		else
		{
			// Inform the user that the PROJECT.XML does not exist
			setErrorTitledPaneStyle(accordionProjectXML);
			// Set text to inform user
			labelProjectXML.setText("The directory '" + projectChecker.getSapelliProjectDir() + "' does not contain a PROJECT.xml and therefore it is not a valid Sapelli project. \n\nMake sure you have a file named PROJECT.xml in this directory.");
			// Expand the accordion
			accordion.setExpandedPane(accordionProjectXML);

			// Disable the Package button
			buttonPackage.setDisable(true);

			// Reset
			setDefaultTitledPaneStyle(accordionImg);
			setDefaultTitledPaneStyle(accordionSnd);
			setDefaultTitledPaneStyle(accordionResources);
		}


		// TODO: 01/06/2017 Continue the validation here
	}

	/**
	 * Set the accordion-default style to a given {@link TitledPane}
	 *
	 * @param pane a {@link TitledPane}
	 */
	private void setDefaultTitledPaneStyle(TitledPane pane)
	{
		// Set style
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-warning", "accordion-success");
		pane.getStyleClass().add("accordion-default");

		// Disable
		pane.setDisable(true);
	}

	/**
	 * Set the accordion-error style to a given {@link TitledPane}
	 *
	 * @param pane a {@link TitledPane}
	 */
	private void setErrorTitledPaneStyle(TitledPane pane)
	{
		// Set style
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-warning", "accordion-success");
		pane.getStyleClass().add("accordion-error");

		// Disable
		pane.setDisable(false);
	}

	/**
	 * Set the accordion-success style to a given {@link TitledPane}
	 *
	 * @param pane a {@link TitledPane}
	 */
	private void setWarningTitledPaneStyle(TitledPane pane)
	{
		// Set style
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-warning", "accordion-success");
		pane.getStyleClass().add("accordion-warning");

		// Disable
		pane.setDisable(false);
	}

	/**
	 * Set the accordion-success style to a given {@link TitledPane}
	 *
	 * @param pane a {@link TitledPane}
	 */
	private void setSuccessTitledPaneStyle(TitledPane pane)
	{
		// Set style
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-warning", "accordion-success");
		pane.getStyleClass().add("accordion-success");

		// Disable
		pane.setDisable(false);
	}

}
