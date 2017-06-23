package uk.ac.ucl.excites.sapelli.packager.ui;

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

}
