package uk.ac.ucl.excites.sapelli.packager.ui;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.text.Text;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * Class to manage the messages on the Packager UI
 * <p>
 * Created by Michalis on 23/06/2017.
 */

@NoArgsConstructor
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
		addString("Your project looks good!\n", projectInfo);
	}

	public void addMissingFiles(List<String> missing)
	{
		addList("The following files are missing:", missing);
	}

	public void addWarnings(List<String> warnings)
	{
		addList("The project contains the following warnings:", warnings);
	}

	public void addErrors(List<String> errors)
	{
		addList("The project contains the following errors:", errors);
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
