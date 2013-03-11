/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;

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
	public void setIn(CollectorUI fv)
	{
		fv.setPhoto(this);
	}
	
}
