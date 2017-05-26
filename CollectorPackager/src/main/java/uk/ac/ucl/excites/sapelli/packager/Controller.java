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

package uk.ac.ucl.excites.sapelli.packager;


import java.io.File;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Controller
{
	public Button buttonBrowse;
	@FXML
	private Label labelDirectory;
	@FXML
	private AnchorPane anchorPane;

	/**
	 * Method to be called when the Browse button is clicked
	 *
	 * @param actionEvent {@link ActionEvent}
	 */
	public void onBrowseButtonClicked(ActionEvent actionEvent)
	{
		Stage stage = (Stage) anchorPane.getScene().getWindow();
		DirectoryChooser directoryChooser = new DirectoryChooser();
		directoryChooser.setTitle("Select directory of project");
		File sapelliProjectDir = directoryChooser.showDialog(stage);

		log.debug("Selected dir: {}", sapelliProjectDir);

		if(sapelliProjectDir != null)
			labelDirectory.setText("Working dir: " + sapelliProjectDir.toString());
	}
}
