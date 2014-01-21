package uk.ac.ucl.excites.collector.project.ui;

import uk.ac.ucl.excites.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.collector.project.model.fields.EndField;
import uk.ac.ucl.excites.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.collector.project.model.fields.MediaField;
import uk.ac.ucl.excites.collector.project.model.fields.OrientationField;
import uk.ac.ucl.excites.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.collector.project.model.fields.lists.MultiListField;

public interface Controller
{
	
	/**
	 * @param cf  the ChoiceField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterChoiceField(ChoiceField cf);

	/**
	 * @param ui	the FieldUI
	 * @param cf  the ChoiceField
	 */
	public void leaveChoiceField(FieldUI ui, ChoiceField cf);
	
	/**
	 * @param af  the MediaField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMediaField(MediaField af);
	
	/**
	 * @param ui	the FieldUI
	 * @param af  the MediaField
	 */
	public void leaveMediaField(FieldUI ui, MediaField mf);
	
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
	 * @param tf  the EditTextField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterTextField(EditTextField tf);
	
	/**
	 * @param tf  the CheckBoxField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterCheckBoxField(CheckBoxField cbf);
	
	/**
	 * @param mlf  the MultiListField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterMultiListField(MultiListField mlf);
	
	/**
	 * @param ef  the EndField
	 * @return whether or not a UI update is required after entering the field
	 */
	public boolean enterEndField(EndField ef);
	
	/**
	 * @return the current ButtonState
	 */
	public ControlsState getButtonsState();
	
}
