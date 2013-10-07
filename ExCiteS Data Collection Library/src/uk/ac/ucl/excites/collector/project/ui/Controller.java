package uk.ac.ucl.excites.collector.project.ui;

import uk.ac.ucl.excites.collector.project.model.*;

public interface Controller
{
	
	/**
	 * @param cf  the ChoiceField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterChoiceField(ChoiceField cf);
	
	/**
	 * @param af  the AudioField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterAudioField(AudioField af);
	
	/**
	 * @param pf  the PhotoField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterPhotoField(PhotoField pf);
	
	/**
	 * @param lf  the LocationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterLocationField(LocationField lf);
	
	/**
	 * @param of  the OrientationField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterOrientationField(OrientationField of);
	
	/**
	 * @param cf  the CancelField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterCancelField(CancelField cf);
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef);
	
	/**
	 * @return the current ButtonState
	 */
	public ButtonsState getButtonsState();
	
}
