/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.model.fields;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.model.Form.Next;
import uk.ac.ucl.excites.sapelli.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.project.ui.Controller;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * Dummy field to represent the end of any form.
 * 
 * @author mstevens
 */
public class EndField extends Field
{
	// Backwards compatibility with v1.x projects:
	static public final String END = "_END";
	static public final String CANCEL = "_CANCEL";
	
	static public List<EndField> GetEndFields(Form form)
	{
		List<EndField> efs = new ArrayList<EndField>();

		// v2.x "End jumps":
		for(Next nxt : Next.values())
		{
			efs.add(new EndField(form, true, nxt));
			efs.add(new EndField(form, false, nxt));	
		}
		// v1.x compatibility:
		// 	Form default _END:
		efs.add(new EndField(form, true));
		// 	_CANCEL:
		efs.add(new EndField(form, false));

		return efs;
	}
	
	// Dynamics
	private boolean save;
	private Next next;
	
	public EndField(Form form, boolean save, Next next)
	{
		super(form, '_' + (save ? "SAVE+" : "") + next.name());
		this.noColumn = true;
		this.save = save;
		this.next = next;
	}
	
	/**
	 * Creates the default EndField for the form, which will always save the record and then to the next behaviour specified by the form's "next"
	 * For v1.x compatiblity only
	 * 
	 * @param form
	 * @param end	whether this is an _END field (true) or an _CANCEL field (false)
	 */
	public EndField(Form form, boolean end)
	{
		super(form, end ? END : CANCEL);
		this.next = end ? form.getNext() : Next.LOOPFORM;
		this.save = end;
	}
	
	/**
	 * @return the save
	 */
	public boolean isSave()
	{
		return save;
	}

	/**
	 * @return the next
	 */
	public Next getNext()
	{
		return next;
	}

	@Override
	protected Column<?> createColumn()
	{
		return null;
	}

	@Override
	public boolean enter(Controller controller)
	{
		return controller.enterEndField(this);
	}
	
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return null; // there is not UI for this field
	}

}
