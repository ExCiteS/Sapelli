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

	public void setChoice(ChoiceField cf);
	
	public void setPhoto(PhotoField pf);
	
	public void setAudio(AudioField af);
	
	public void setLocation(LocationField lf);
	
	public void setOrientation(OrientationField of);
	
	public void setEndField(EndField endF);
	
	public void setCancelField(CancelField cancelF);
	
}
