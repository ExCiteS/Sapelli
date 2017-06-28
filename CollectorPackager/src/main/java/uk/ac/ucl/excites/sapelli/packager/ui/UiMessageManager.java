package uk.ac.ucl.excites.sapelli.packager.ui;

import java.util.List;

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
	private StringBuilder messageBuilder = new StringBuilder();

	public void clear()
	{
		state = State.DEFAULT;
		messageBuilder = new StringBuilder();
	}

	public String getMessages()
	{
		return messageBuilder.toString();
	}

	public void addMessages(String message)
	{
		messageBuilder.append(message).append("\n");
	}

	public void addMissingFiles(List<String> missing)
	{
		if(missing.isEmpty())
			return;

		if(messageBuilder.length() > 0)
			messageBuilder.append("\n");

		messageBuilder.append("The following files are missing:\n\n");
		for(String file : missing)
			messageBuilder.append(file).append("\n");
	}

	public void addWarnings(List<String> warnings)
	{
		if(warnings.isEmpty())
			return;

		if(messageBuilder.length() > 0)
			messageBuilder.append("\n");

		messageBuilder.append("The project contains the following warnings:\n\n");
		for(String warning : warnings)
			messageBuilder.append(warning).append("\n");
	}

	public void addErrors(List<String> errors)
	{
		if(errors.isEmpty())
			return;

		if(messageBuilder.length() > 0)
			messageBuilder.append("\n");

		messageBuilder.append("The project contains the following errors:\n\n");
		for(String error : errors)
			messageBuilder.append(error).append("\n");
	}

}
