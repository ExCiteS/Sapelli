/**
 * 
 */
package uk.ac.ucl.excites.collector.project.ui;

import uk.ac.ucl.excites.collector.project.model.*;

/**
 * @author mstevens
 *
 */
public interface CollectorUI
{

	public void setChoice(Choice cf);
	
	public void setPhoto(Photo pf);
	
	public void setAudio(Audio af);
	
	public void setLocation(LocationField lf);
	
	public void setOrientation(OrientationField of);
	
}
