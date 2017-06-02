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
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectChecker;

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
			accordion.setExpandedPane(accordionProjectXML);
			// TODO: 02/06/2017 Check Projects
		}
		else
		{
			// Inform the user that the PROJECT.XML does not exist
			setErrorTitledPaneStyle(accordionProjectXML);
			// TODO: 02/06/2017 Inform user

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
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-success");
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
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-success");
		pane.getStyleClass().add("accordion-error");

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
		pane.getStyleClass().removeAll("accordion-default", "accordion-error", "accordion-success");
		pane.getStyleClass().add("accordion-success");

		// Disable
		pane.setDisable(false);
	}

}
