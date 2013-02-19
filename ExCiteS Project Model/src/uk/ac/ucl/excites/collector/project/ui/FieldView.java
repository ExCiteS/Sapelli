/**
 * 
 */
package uk.ac.ucl.excites.collector.project.ui;

import uk.ac.ucl.excites.collector.project.model.*;

/**
 * @author mstevens
 *
 */
public interface FieldView
{

	public void setChoice(Choice cf);
	
	public void setPhoto(Photo pf);
	
	public void setAudio(Audio af);
	
	public void setLocation(LocationField fl);
	
}
