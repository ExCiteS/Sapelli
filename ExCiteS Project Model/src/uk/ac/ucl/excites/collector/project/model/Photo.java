/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.FieldView;

/**
 * @author Michalis Vitos
 *
 */
public class Photo extends MediaAttachment
{

	public Photo(String id)
	{
		super(id);
	}

	@Override
	public void setIn(FieldView fv)
	{
		fv.setPhoto(this);
	}
	
}
