package uk.ac.ucl.excites.sapelli.packager.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javafx.scene.text.Text;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * Class to manage the messages on the Packager UI
 * <p>
 * Created by Michalis on 23/06/2017.
 */

@Slf4j
public class UiMessageManager
{
	public enum State
	{
		DEFAULT,
		SUCCESS,
		WARNING,
		ERROR
	}

	@Getter
	@Setter
	private State state = State.DEFAULT;
	@Getter
	private List<Text> messages = new ArrayList<>();
	private ResourceBundle resources;

	public UiMessageManager(ResourceBundle resources)
	{
		this.resources = resources;
	}

	public void clear()
	{
		state = State.DEFAULT;
		messages = new ArrayList<>();
	}

	public void addMessage(String message)
	{
		final Text text = new Text(message + "\n");
		messages.add(text);
	}

	public void addProject(String projectInfo)
	{
		addString(resources.getString("project_looks_good"), projectInfo);
	}

	public void addMissingFiles(List<String> missing)
	{
		addList(resources.getString("files_missing"), missing);
	}

	public void addWarnings(List<String> warnings)
	{
		addList(resources.getString("warnings"), warnings);
	}

	public void addErrors(List<String> errors)
	{
		addList(resources.getString("errors"), errors);
	}

	private void addString(String title, String content)
	{
		// If the list is not empty, add some whitespace
		if(!messages.isEmpty())
			messages.add(new Text("\n"));

		final Text titleText = new Text(title + "\n");
		titleText.setStyle("-fx-font-weight: bold");
		messages.add(titleText);
		messages.add(new Text(content + "\n"));
	}

	private void addList(String title, List<String> list)
	{
		if(list.isEmpty())
			return;

		// If the list is not empty, add some whitespace
		if(!messages.isEmpty())
			messages.add(new Text("\n"));

		final Text titleText = new Text(title + "\n");
		titleText.setStyle("-fx-font-weight: bold");
		messages.add(titleText);
		for(String s : list)
			messages.add(new Text(s + "\n"));
	}

	/**
	 * Get all the messages as a String, ignore empty lines
	 *
	 * @return
	 */
	public String toString()
	{
		StringBuilder builder = new StringBuilder();

		for(Text text : messages)
		{
			String textString = text.getText().trim();
			if(!textString.equalsIgnoreCase("\n"))
				builder.append(textString).append("\n");
		}

		return builder.toString().trim();
	}

}
