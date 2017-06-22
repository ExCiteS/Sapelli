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

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;

/**
 * Entry point for the Packager Javafx application. This class setups the UI and launches the app.
 * <p>
 * Created by Michalis on 25/05/2017.
 */
@Slf4j
public class Packager extends Application
{
	@Override
	public void start(Stage primaryStage) throws Exception
	{
		Parent root = FXMLLoader.load(getClass().getResource("/PackagerUI.fxml"));
		primaryStage.setTitle("Sapelli Packager");
		primaryStage.setScene(new Scene(root));
		primaryStage.getIcons().add(new Image("/icon/SapelliPackager.png"));
		primaryStage.show();
	}

	public static void main(String[] args)
	{
		log.info("Sapelli Packer is starting!");

		try
		{
			launch(args);
		}
		catch(Exception e)
		{
			log.error("Error while trying to load JavaFX: ", e);
		}
	}
}
