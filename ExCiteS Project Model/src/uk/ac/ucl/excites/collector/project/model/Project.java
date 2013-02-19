package uk.ac.ucl.excites.collector.project.model;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mstevens
 *
 */
public class Project
{
	private String name;
	private List<Form> forms;
	
	public Project(String name)
	{
		this.forms = new ArrayList<Form>();
		this.name = name;
	}
	
	public void addForm(Form frm)
	{
		forms.add(frm);
	}
	
	public List<Form> getForms()
	{
		return forms;
	}
	
	public String getName()
	{
		return name;
	}
	
}

