package uk.ac.ucl.excites.collector.model;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mstevens
 *
 */
public class Project
{
	
	private ArrayList<Form> forms;
	
	public Project()
	{
		this.forms = new ArrayList<Form>();
	}
	
	public void addForm(Form frm)
	{
		forms.add(frm);
	}
	
	public List<Form> getForms()
	{
		return forms;
	}
	
}

